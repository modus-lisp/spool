;;;; radio.lisp — a live MP3 stream (Icecast / Shoutcast), playing.
;;;;
;;;; Same output contract as play.lisp — a SOURCE THUNK, polled on the mixer's 20 ms
;;;; clock, that must never block or signal — so everything network-shaped happens on
;;;; a FETCHER THREAD and the mixer only ever touches memory.  The two meet in a byte
;;;; queue: the fetcher appends, the mixer's top-up drains it into the decoder's window.
;;;;
;;;; The queue DROPS ITS OLDEST bytes on overflow.  A live stream has no "behind": if
;;;; the consumer stalls (paused, or the mixer is starved) the right thing is to lose
;;;; the stale audio and stay near live, not to block the socket until the server
;;;; disconnects us.  Resume from pause is the same idea taken to its end — REJOIN
;;;; throws the buffer away.
;;;;
;;;; Decoding reuses reed's MP3 player unchanged by handing it a fixed WINDOW of bytes
;;;; that we compact and refill in place.  Reed's decoder keeps its bit reservoir
;;;; separately from the byte window, so sliding the window mid-stream is invisible to
;;;; it.  It scans for frame sync in [pos, end), and a frame that runs past END is
;;;; skipped (its bytes are lost), so the window is topped up long before it runs dry
;;;; and playback goes to :buffering rather than decoding the last few KB.
;;;;
;;;; ICY metadata: we ask for it (Icy-MetaData: 1); the server then inserts a length
;;;; byte (x16) plus a "StreamTitle='...';" block after every METAINT audio bytes.  Those
;;;; bytes are NOT audio and are removed here, before the decoder sees anything.

(in-package #:spool)

(defparameter *radio-queue-cap* (* 128 1024))
(defparameter *radio-window-size* (* 256 1024))
(defparameter *radio-low-water* (* 16 1024) "top the window up below this many bytes ahead")
(defparameter *radio-start-ahead* (* 24 1024) "bytes ahead before :buffering becomes :playing")
(defparameter *radio-underrun* (* 4 1024) "below this ahead, stop decoding and rebuffer")
(defparameter *radio-resume-ahead* (* 16 1024))
(defparameter *radio-max-redirects* 5)
(defparameter *radio-max-attempts* 3 "connects that never deliver audio before giving up")

(defstruct (radio (:constructor %make-radio))
  (url nil)
  (name nil)
  (genre nil)
  (title nil)                           ; ICY StreamTitle — what is playing right now
  (thread nil)
  (stream nil)
  (stop nil)
  (queue (make-array *radio-queue-cap* :element-type '(unsigned-byte 8))
   :type (simple-array (unsigned-byte 8) (*)))
  (qfill 0 :type fixnum)
  (qlock (sb-thread:make-mutex :name "spool-radio-queue"))
  (win (make-array *radio-window-size* :element-type '(unsigned-byte 8))
   :type (simple-array (unsigned-byte 8) (*)))
  (reed nil)
  (buffering-since 0)
  (metaint nil)
  (connects 0 :type fixnum)
  (bytes-in 0 :type integer)
  (player nil))

;;; ---- the queue --------------------------------------------------------------

(defun %radio-push (r buf n)
  "Append N bytes of BUF, dropping the oldest on overflow.  Never blocks on the consumer."
  (declare (type (simple-array (unsigned-byte 8) (*)) buf) (type fixnum n))
  (let ((q (radio-queue r)) (cap (length (radio-queue r))))
    (sb-thread:with-mutex ((radio-qlock r))
      (when (> n cap)                   ; keep only the newest CAP bytes
        (setf buf (subseq buf (- n cap) n) n cap))
      (let ((over (- (+ (radio-qfill r) n) cap)))
        (when (plusp over)
          (replace q q :start2 over :end2 (radio-qfill r))
          (decf (radio-qfill r) over)))
      (replace q buf :start1 (radio-qfill r) :end2 n)
      (incf (radio-qfill r) n)
      (incf (radio-bytes-in r) n))))

(defun %radio-clear-queue (r)
  (sb-thread:with-mutex ((radio-qlock r)) (setf (radio-qfill r) 0)))

;;; ---- the decoder window -----------------------------------------------------

(defun %radio-fresh-reed (p r)
  "A new reed player over R's window, with the window empty."
  (let* ((rp (reed:make-mp3-player (radio-win r)
                                   :rate (player-rate p)
                                   :frame-samples (player-frame-samples p)
                                   :gain (player-gain p)))
         (d (reed::player-decoder rp)))
    (setf (reed::decoder-pos d) 0
          (reed::decoder-end d) 0)
    (setf (radio-reed r) rp)
    rp))

(defun %radio-ahead (r)
  (let ((d (reed::player-decoder (radio-reed r))))
    (- (reed::decoder-end d) (reed::decoder-pos d))))

(defun %radio-topup (r)
  "Refill the decoder window from the queue when it is running low."
  (let* ((d (reed::player-decoder (radio-reed r)))
         (win (radio-win r)))
    (when (< (- (reed::decoder-end d) (reed::decoder-pos d)) *radio-low-water*)
      (let ((pos (reed::decoder-pos d)) (end (reed::decoder-end d)))
        (when (plusp pos)
          (replace win win :start2 pos :end2 end)
          (setf end (- end pos) pos 0))
        (sb-thread:with-mutex ((radio-qlock r))
          (let* ((q (radio-queue r))
                 (n (min (radio-qfill r) (- (length win) end))))
            (when (plusp n)
              (replace win q :start1 end :end2 n)
              (incf end n)
              (replace q q :start2 n :end2 (radio-qfill r))
              (decf (radio-qfill r) n))))
        (setf (reed::decoder-pos d) pos
              (reed::decoder-end d) end)))))

(defun %radio-chain-ok-p (win start end)
  "True when a run of consecutive, mutually consistent frame headers begins at START.
A single header match proves little in a stream joined mid-frame: audio bytes spell a
plausible header often enough that the decoder would lock onto one, decode garbage and
skip the real frames behind it.  Three in a row, each landing exactly where the last one
says the next begins, is not a coincidence."
  (let ((first nil) (pos start))
    (dotimes (i 3 t)
      (unless (and (< (+ pos 4) end)
                   (reed::valid-frame-sync-p (aref win pos) (aref win (1+ pos))))
        (return nil))
      (let ((h (reed::parse-frame-header (reed::u32be win pos))))
        (unless (and h (>= (reed::fh-frame-length h) 4)
                     (<= (+ pos (reed::fh-frame-length h)) end))
          (return nil))
        (if first
            (unless (and (eq (reed::fh-version h) (reed::fh-version first))
                         (= (reed::fh-layer h) (reed::fh-layer first))
                         (= (reed::fh-samplerate-index h) (reed::fh-samplerate-index first))
                         (= (reed::fh-channels h) (reed::fh-channels first)))
              (return nil))
            (setf first h))
        (incf pos (reed::fh-frame-length h))))))

(defun %radio-align (r)
  "Move the decoder onto a validated frame boundary.  Returns true when it found one;
otherwise discards what it examined and returns NIL, so the caller keeps buffering."
  (let* ((d (reed::player-decoder (radio-reed r)))
         (win (radio-win r))
         (end (reed::decoder-end d)))
    (loop for i from (reed::decoder-pos d) below (- end 4)
          when (and (= (aref win i) #xFF) (%radio-chain-ok-p win i end))
            do (setf (reed::decoder-pos d) i)
               (return t)
          finally
             ;; Keep the last few bytes: a header may straddle the end of what we have.
             (setf (reed::decoder-pos d) (max (reed::decoder-pos d) (- end 4)))
             (return nil))))

(defun %radio-rebuffer (p r)
  (%radio-fresh-reed p r)
  (setf (radio-buffering-since r) (get-internal-real-time)
        (player-state p) :buffering))

(defun %radio-frame (p)
  "One mixer frame of the live stream, or NIL.  Caller holds the player lock; this is
the mixer thread, so nothing here may block."
  (let ((r (player-radio p)))
    (when (and r (member (player-state p) '(:playing :buffering)))
      (%radio-topup r)
      (let ((ahead (%radio-ahead r)))
        (ecase (player-state p)
          (:buffering
           (if (and (or (>= ahead *radio-start-ahead*)
                        (and (>= ahead (* 8 1024))
                             (> (- (get-internal-real-time) (radio-buffering-since r))
                                (* 5 internal-time-units-per-second))))
                    (%radio-align r))
               (setf (player-state p) :playing)
               (return-from %radio-frame nil)))
          (:playing
           (when (< ahead *radio-underrun*)
             (setf (radio-buffering-since r) (get-internal-real-time)
                   (player-state p) :buffering)
             (return-from %radio-frame nil)))))
      (let ((f (handler-case (reed:player-next-frame (radio-reed r))
                 ;; A damaged frame (a dropped span, a false sync) can make the decoder
                 ;; fault rather than refuse.  The decoder has already stepped past it,
                 ;; so one silent frame is the whole cost.
                 (error () :bad))))
        (cond ((eq f :bad) nil)
              (f (incf (player-emitted p) (length f)) f)
              (t ;; Garbage, not a real end — a live stream has none.  Start over.
               (%radio-rebuffer p r)
               nil))))))

(defun %radio-rejoin (p)
  "Drop everything buffered and rejoin the live edge.  Caller holds the player lock."
  (let ((r (player-radio p)))
    (when r
      (%radio-clear-queue r)
      (%radio-rebuffer p r))))

;;; ---- the network ------------------------------------------------------------

(defun %radio-header-name (headers &rest names)
  (dolist (n names)
    (let ((v (weft.fetch::get-header headers n)))
      (when (and v (plusp (length (string-trim " " v)))) (return (string-trim " " v))))))

(defun %radio-decode-text (octets)
  (handler-case (sb-ext:octets-to-string octets :external-format :utf-8)
    (error () (map 'string #'code-char octets))))

(defun %radio-parse-title (octets)
  "The StreamTitle out of an ICY metadata block, or NIL."
  (let* ((s (%radio-decode-text octets))
         (key "StreamTitle='")
         (a (search key s)))
    (when a
      (let* ((from (+ a (length key)))
             ;; The value ends at the LAST "';" — titles contain quotes.
             (b (or (search "';" s :start2 from) (length s))))
        (let ((v (string-trim '(#\Space #\Nul) (subseq s from b))))
          (when (plusp (length v)) v))))))

(defun %radio-fail (r why)
  (let ((p (radio-player r)))
    (when p
      (sb-thread:with-mutex ((player-lock p))
        (when (eq (player-radio p) r)
          (setf (player-state p) :error (player-error p) why))))
    (setf (radio-stop r) t)))

(define-condition radio-fatal (error)
  ((why :initarg :why :reader radio-fatal-why))
  (:report (lambda (c s) (write-string (radio-fatal-why c) s))))

(defun %radio-read-body (stream headers)
  (let ((len (ignore-errors (parse-integer (or (weft.fetch::get-header headers "content-length") "")
                                           :junk-allowed t))))
    (%radio-decode-text
     (weft.fetch::read-n-bytes stream (min 65536 (or len 4096))))))

(defun %radio-playlist-url (text)
  "The first http(s) URL in a .pls or .m3u body."
  (with-input-from-string (in text)
    (loop for line = (read-line in nil)
          while line
          do (let* ((l (string-trim '(#\Space #\Return #\Tab) line))
                    (eq (position #\= l))
                    (v (if (and eq (>= (length l) 5) (string-equal "File" l :end2 4))
                           (string-trim " " (subseq l (1+ eq)))
                           l)))
               (when (and (>= (length v) 7)
                          (or (string-equal "http://" v :end2 7)
                              (and (>= (length v) 8) (string-equal "https://" v :end2 8))))
                 (return v))))))

(defun %radio-request (url)
  (let ((path (concatenate 'string
                           (let ((s (weft.url:pathname-str url))) (if (string= s "") "/" s))
                           (weft.url:search-str url))))
    (weft.fetch::%ascii-octets
     (format nil "GET ~a HTTP/1.0~c~cHost: ~a~c~cUser-Agent: spool/1 (radio)~c~cAccept: */*~c~cIcy-MetaData: 1~c~cConnection: close~c~c~c~c"
             path #\Return #\Newline (weft.url:host-str url) #\Return #\Newline
             #\Return #\Newline #\Return #\Newline #\Return #\Newline
             #\Return #\Newline #\Return #\Newline))))

(defun %radio-open (r url-string)
  "Connect, follow redirects and playlists.  Returns (values stream headers), or signals
RADIO-FATAL for an answer that reconnecting cannot fix."
  (let ((target url-string))
    (dotimes (hop (1+ *radio-max-redirects*) (error 'radio-fatal :why "too many redirects"))
      (let* ((url (or (weft.url:parse target) (error 'radio-fatal :why "bad URL")))
             (scheme (weft.url:url-scheme url)))
        (unless (member scheme '("http" "https") :test #'string-equal)
          (error 'radio-fatal :why (format nil "unsupported scheme ~a" scheme)))
        (let* ((port (or (weft.url:url-port url) (weft.fetch::scheme-default-port scheme)))
               (stream (weft.fetch::open-stream-for scheme (weft.url:hostname url) port 10)))
          (setf (radio-stream r) stream)
          (write-sequence (%radio-request url) stream)
          (finish-output stream)
          (let* ((status (weft.fetch::parse-status-line (weft.fetch::read-header-line stream)))
                 (headers (weft.fetch::read-headers stream))
                 (ctype (string-downcase
                         (or (weft.fetch::get-header headers "content-type") ""))))
            (cond
              ((<= 300 status 399)
               (let ((loc (weft.fetch::get-header headers "location")))
                 (ignore-errors (close stream))
                 (unless loc (error 'radio-fatal :why "redirect without a location"))
                 (setf target (let ((u (weft.url:parse loc target)))
                                (if u (weft.url:href u) loc)))))
              ((<= 400 status 499)
               (ignore-errors (close stream))
               (error 'radio-fatal :why (format nil "server said ~d" status)))
              ((not (= status 200))
               (ignore-errors (close stream))
               (error "server said ~d" status))
              ((some (lambda (k) (search k ctype))
                     '("scpls" "mpegurl" "text/plain" "pls+xml"))
               (let ((next (%radio-playlist-url (%radio-read-body stream headers))))
                 (ignore-errors (close stream))
                 (unless next (error 'radio-fatal :why "playlist had no stream URL"))
                 (setf target next)))
              ((some (lambda (k) (search k ctype)) '("aac" "ogg" "opus" "flac" "mp4"))
               (ignore-errors (close stream))
               (error 'radio-fatal :why (format nil "no decoder for ~a" ctype)))
              (t (return-from %radio-open (values stream headers))))))))))

(defun %radio-read-exactly (stream buf n)
  "Fill BUF[0,N) or return NIL at end of stream."
  (let ((got (read-sequence buf stream :end n)))
    (= got n)))

(defun %radio-pump (r stream metaint)
  "Copy audio into the queue until the stream ends or R is stopped.  Returns when the
connection is over; the caller decides whether to reconnect."
  (let ((buf (make-array 2048 :element-type '(unsigned-byte 8)))
        (meta (make-array 4096 :element-type '(unsigned-byte 8)))
        (left (or metaint most-positive-fixnum)))
    (loop until (radio-stop r)
          do (let ((n (min 2048 left)))
               (when (plusp n)
                 (let ((got (read-sequence buf stream :end n)))
                   (when (zerop got) (return))
                   (%radio-push r buf got)
                   (decf left got)
                   (when (< got n) (return))))
               (when (zerop left)
                 (let ((lb (read-byte stream nil nil)))
                   (unless lb (return))
                   (let ((len (* 16 lb)))
                     (when (plusp len)
                       (unless (%radio-read-exactly stream meta len) (return))
                       (let ((title (%radio-parse-title (subseq meta 0 len))))
                         (when title (setf (radio-title r) title))))))
                 (setf left metaint))))))

(defun %radio-sleep (r seconds)
  (loop repeat (round (* seconds 10))
        until (radio-stop r)
        do (sleep 0.1)))

(defun %radio-run (r)
  (let ((backoff 1) (failures 0))
    (loop until (radio-stop r)
          do (let ((streamed nil))
               (handler-case
                   (multiple-value-bind (stream headers) (%radio-open r (radio-url r))
                     (unwind-protect
                          (let ((mi (ignore-errors
                                     (parse-integer (or (weft.fetch::get-header headers "icy-metaint") "")
                                                    :junk-allowed t))))
                            (incf (radio-connects r))
                            (setf (radio-metaint r) mi
                                  (radio-name r) (or (%radio-header-name headers "icy-name")
                                                     (radio-name r))
                                  (radio-genre r) (%radio-header-name headers "icy-genre"))
                            (let ((p (radio-player r)))
                              (when (and p (radio-name r))
                                (setf (player-title p) (radio-name r))))
                            (let ((before (radio-bytes-in r)))
                              (%radio-pump r stream (and mi (plusp mi) mi))
                              (setf streamed (> (radio-bytes-in r) (+ before 8192)))))
                       (ignore-errors (close stream))
                       (setf (radio-stream r) nil)))
                 (radio-fatal (c)
                   (%radio-fail r (radio-fatal-why c)))
                 (serious-condition (c)
                   (unless (radio-stop r)
                     (incf failures)
                     (when (and (zerop (radio-connects r)) (>= failures *radio-max-attempts*))
                       (%radio-fail r (princ-to-string c))))))
               (unless (radio-stop r)
                 (if streamed
                     (setf backoff 1 failures 0)
                     (setf backoff (min 15 (* 2 backoff))))
                 (%radio-sleep r backoff))))))

;;; ---- the player-facing side -------------------------------------------------

(defun %radio-stop (p)
  "Stop and forget P's radio, if it has one.  Called WITHOUT the player lock: it joins
the fetcher, which may itself want the lock to report a failure."
  (let ((r (sb-thread:with-mutex ((player-lock p))
             (prog1 (player-radio p) (setf (player-radio p) nil)))))
    (when r
      (setf (radio-stop r) t)
      ;; The fetcher is usually parked in a read; closing the stream is what wakes it.
      (let ((s (radio-stream r))) (when s (ignore-errors (close s :abort t))))
      (let ((th (radio-thread r)))
        (when (and th (not (eq th sb-thread:*current-thread*)))
          (ignore-errors (sb-thread:join-thread th :default nil :timeout 2))))
      (sb-thread:with-mutex ((player-lock p))
        (setf (player-reed p) nil)))
    r))

(defun play-url (p url &key name)
  "Play the live MP3 stream at URL on P.  Returns at once: the connection is made on a
fetcher thread and the state is :buffering until enough audio has arrived, then
:playing.  A refusal (404, a codec we have no decoder for) arrives later as state
:error with the reason in PLAYER-ERROR — there is nothing to return it through now."
  (%radio-stop p)
  (let ((r (%make-radio :url url :name name :player p)))
    (sb-thread:with-mutex ((player-lock p))
      (setf (player-radio p) r
            (player-state p) :buffering
            (player-error p) nil
            (player-episode p) nil
            (player-path p) nil
            (player-bytes p) nil
            (player-title p) (or name url)
            (player-duration p) nil
            (player-emitted p) 0
            (player-base p) 0.0d0)
      (%radio-fresh-reed p r)
      (setf (radio-buffering-since r) (get-internal-real-time)))
    (setf (radio-thread r)
          (sb-thread:make-thread (lambda () (%radio-run r)) :name "spool-radio"))
    url))

(defun player-live-p (p) (and (player-radio p) t))

(defun player-stream-title (p)
  "What the station says is playing (ICY StreamTitle), or NIL."
  (let ((r (player-radio p))) (and r (radio-title r))))

(defun player-station (p)
  (let ((r (player-radio p))) (and r (radio-name r))))
