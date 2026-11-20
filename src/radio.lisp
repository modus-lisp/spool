;;;; radio.lisp — a live MP3 stream (Icecast / Shoutcast), playing.
;;;;
;;;; Same output contract as play.lisp — a SOURCE THUNK, polled on the mixer's 20 ms
;;;; clock, that must never block or signal — so everything network-shaped happens on
;;;; a thread of its own and the mixer only ever touches memory.
;;;;
;;;; ONE WORKER OWNS THE STREAM, AND SHARES NOTHING.  A station is one thread: it connects,
;;;; reads, strips the ICY metadata, and decodes, and everything it uses -- the socket, the
;;;; byte window, reed's decoder -- is its own.  What the player sees arrives as MESSAGES in
;;;; a box the player made: each decoded frame, (:PCM frame); what the station calls itself
;;;; and what it says is on, (:NAME ...) (:GENRE ...) (:TITLE ...); a refusal, (:ERROR why).
;;;; The player says (:STOP) and (:REJOIN) the same way.  So it runs where threads share no
;;;; state (modus, the phone: a thread may not store into memory another made), and the
;;;; mixer's thread never waits on a lock the network holds.  It used to be a fetcher thread
;;;; and a decoder thread writing into the player's own structures under its lock.
;;;;
;;;; THE PLAYER'S SIDE BUFFERS.  The source drains the box on every mixer tick, keeps the
;;;; frames in a queue of its own, and starts (:BUFFERING -> :PLAYING) once *RADIO-START-
;;;; FRAMES* are waiting; an empty queue is an underrun, back to buffering.  The queue DROPS
;;;; ITS OLDEST beyond *RADIO-MAX-FRAMES*: a live stream has no "behind" -- the right thing
;;;; after a stall is to stay near live -- and a paused station throws frames away as they
;;;; come.  Resume REJOINS: the queue is emptied and the worker starts its decoder afresh.
;;;;
;;;; Decoding reuses reed's MP3 player unchanged by handing it a fixed WINDOW of bytes that
;;;; the worker compacts and refills in place.  Reed keeps its bit reservoir separately from
;;;; the byte window, so sliding the window mid-stream is invisible to it.  A frame that runs
;;;; past the window's end is skipped, so the worker decodes only while a few KB are ahead.
;;;;
;;;; ICY metadata: we ask for it (Icy-MetaData: 1); the server then inserts a length byte
;;;; (x16) plus a "StreamTitle='...';" block after every METAINT audio bytes.  Those bytes
;;;; are NOT audio and are removed before the decoder sees anything.

(in-package #:spool)

(defparameter *radio-window-size* (* 256 1024))
(defparameter *radio-decode-margin* (* 4 1024)
  "Bytes the worker keeps ahead of the decoder: a frame is decoded only once whole.")
(defparameter *radio-align-ahead* (* 8 1024) "bytes in hand before the first frame is sought")
;; 1 s, not 300 ms: a live stream arrives at exactly real time, so this is all the slack there is
;; against the network (and the phone's speaker loop takes a cushion of frames at once on top).
;; With 15, Groove Salad on the phone stopped and restarted every second or so.
(defparameter *radio-start-frames* 50 "frames waiting (20 ms each) before buffering becomes playing")
(defparameter *radio-max-frames* 250 "frames the player keeps at most (5 s); older ones are dropped")
(defparameter *radio-max-redirects* 5)
(defparameter *radio-max-attempts* 3 "connects that never deliver audio before giving up")

;;; ---- crossing threads ---------------------------------------------------------
;;;
;;; modus threads share no state: a value meant for another thread is copied into shared
;;; memory (MODUS:SHARE), and a typed array -- a decoded frame -- is made shared to begin with.
;;; Elsewhere both are the identity.

(defun %share (x) #+modus (modus:share x) #-modus x)

(defun %shared-copy (v)
  #+modus (let ((s (modus:make-shared-array (length v) :element-type (array-element-type v))))
            (replace s v) s)
  #-modus v)

(defstruct (rbox (:constructor %make-rbox))
  (lock (sb-thread:make-mutex :name "spool-radio-box"))
  (items '())
  ;; THE OUT BOX'S BACK-PRESSURE: frames the player has finished with (played, dropped, thrown away
  ;; paused or on a rejoin), counted by the player's thread -- a fixnum, which any thread may store.
  ;; The worker compares it with what it has posted and stops reading the socket while the player
  ;; has a queue's worth waiting (see %W-PUMP).
  (taken 0 :type fixnum))

(defun %make-box () (%share (%make-rbox)))

(defun %post (box msg)
  (sb-thread:with-mutex ((rbox-lock box))
    (setf (rbox-items box) (%share (cons msg (rbox-items box)))))
  msg)

(defun %take (box)
  "Every message, oldest first."
  (sb-thread:with-mutex ((rbox-lock box))
    (prog1 (reverse (rbox-items box)) (setf (rbox-items box) nil))))

;;; ---- the player's side --------------------------------------------------------

(defstruct (radio (:constructor %make-radio))
  (url nil)
  (name nil)
  (genre nil)
  (title nil)                           ; ICY StreamTitle — what is playing right now
  (ctl nil)                             ; box: player -> worker
  (out nil)                             ; box: worker -> player
  (thread nil)
  (pcm '())                             ; decoded frames ready for the mixer, oldest first
  (pcm-n 0 :type fixnum)
  (buffering-since 0)
  (player nil))

(defun %radio-clear-pcm (r)
  (incf (rbox-taken (radio-out r)) (radio-pcm-n r))
  (setf (radio-pcm r) '() (radio-pcm-n r) 0))

;;; ---- what the stream is doing, for whoever asks (vessel's KILN_AUDIO_STATS) ------------------
;;;
;;; *RADIO-STATS*, when set, is a RADIO-STATS the caller made on the thread that ticks the mixer
;;; (which is where %RADIO-DRAIN and %RADIO-FRAME run); they count into it, the caller reports and
;;; resets it.  Unset, nothing is counted.

(defstruct (radio-stats (:constructor make-radio-stats))
  (in 0 :type fixnum)             ; frames the worker sent
  (out 0 :type fixnum)            ; frames handed to the mixer
  (waiting 0 :type fixnum)        ; mixer ticks answered with nothing while buffering
  (rebuffers 0 :type fixnum)      ; playing -> buffering: the queue ran dry
  (dropped 0 :type fixnum)        ; frames thrown away past *RADIO-MAX-FRAMES*
  (qmin most-positive-fixnum :type fixnum) (qmax 0 :type fixnum)   ; queue depth after a frame went out
  (dec-us 0 :type fixnum) (dec-max 0 :type fixnum)                 ; the worker's decode time a frame
  (jump 0 :type fixnum)           ; largest step from one frame's last sample to the next's first
  (last 0 :type fixnum)
  (ms-min most-positive-fixnum :type fixnum) (ms-max 0 :type fixnum))  ; a frame's mean square

(defvar *radio-stats* nil)

(defun radio-stats-line (s)
  "One line of S's counts."
  (format nil "radio in ~D out ~D waiting ~D rebuffers ~D dropped ~D queue ~D..~D decode mean ~D us max ~D us, seam max ~D, rms ~D..~D"
          (radio-stats-in s) (radio-stats-out s) (radio-stats-waiting s) (radio-stats-rebuffers s)
          (radio-stats-dropped s)
          (if (= (radio-stats-qmin s) most-positive-fixnum) -1 (radio-stats-qmin s)) (radio-stats-qmax s)
          (floor (radio-stats-dec-us s) (max 1 (radio-stats-in s))) (radio-stats-dec-max s)
          (radio-stats-jump s)
          (if (= (radio-stats-ms-min s) most-positive-fixnum) -1 (isqrt (radio-stats-ms-min s)))
          (isqrt (radio-stats-ms-max s))))

(defun %radio-stats-out (s f q)
  (incf (radio-stats-out s))
  (setf (radio-stats-qmin s) (min (radio-stats-qmin s) q)
        (radio-stats-qmax s) (max (radio-stats-qmax s) q))
  (when (plusp (length f))
    (setf (radio-stats-jump s) (max (radio-stats-jump s) (abs (- (aref f 0) (radio-stats-last s)))))
    (setf (radio-stats-last s) (aref f (1- (length f))))
    (let ((sum 0))
      (declare (type fixnum sum))
      (dotimes (i (length f)) (let ((x (aref f i))) (setf sum (+ sum (* x x)))))
      (let ((ms (floor sum (length f))))
        (setf (radio-stats-ms-min s) (min (radio-stats-ms-min s) ms)
              (radio-stats-ms-max s) (max (radio-stats-ms-max s) ms))))))

(defun %radio-drain (p r)
  "Take what the worker sent.  Caller holds the player lock."
  (dolist (m (%take (radio-out r)))
    (case (first m)
      (:pcm (let ((s *radio-stats*))
              (when s
                (incf (radio-stats-in s))
                (let ((us (or (third m) 0)))
                  (incf (radio-stats-dec-us s) us)
                  (setf (radio-stats-dec-max s) (max (radio-stats-dec-max s) us))))
              (if (eq (player-state p) :paused)
                  (incf (rbox-taken (radio-out r)))
                  (progn
                    (setf (radio-pcm r) (nconc (radio-pcm r) (list (second m))))
                    (incf (radio-pcm-n r))
                    (when (> (radio-pcm-n r) *radio-max-frames*)
                      (pop (radio-pcm r)) (decf (radio-pcm-n r))
                      (incf (rbox-taken (radio-out r)))
                      (when s (incf (radio-stats-dropped s))))))))
      (:title (setf (radio-title r) (second m)))
      (:genre (setf (radio-genre r) (second m)))
      (:name (setf (radio-name r) (second m) (player-title p) (second m)))
      (:error (setf (player-state p) :error (player-error p) (second m))))))

(defun %radio-frame (p)
  "One mixer frame of the live stream, or NIL.  Caller holds the player lock; this is the
mixer thread, so nothing here waits on the network: it takes the worker's messages, keeps
the buffering state, and pops a frame.  The position counts here, at play time."
  (let ((r (player-radio p)))
    (when r
      (%radio-drain p r)
      (case (player-state p)
        (:buffering
         (when (>= (radio-pcm-n r) *radio-start-frames*)
           (setf (player-state p) :playing))
         (when *radio-stats* (incf (radio-stats-waiting *radio-stats*)))
         nil)
        (:playing
         (if (null (radio-pcm r))
             (progn (setf (radio-buffering-since r) (get-internal-real-time)
                          (player-state p) :buffering)
                    (when *radio-stats* (incf (radio-stats-rebuffers *radio-stats*)))
                    nil)
             (let ((f (pop (radio-pcm r))))
               (decf (radio-pcm-n r))
               (incf (rbox-taken (radio-out r)))
               (incf (player-emitted p) (length f))
               (when *radio-stats* (%radio-stats-out *radio-stats* f (radio-pcm-n r)))
               f)))))))

(defun %radio-rejoin (p)
  "Drop everything buffered and rejoin the live edge.  Caller holds the player lock."
  (let ((r (player-radio p)))
    (when r
      (%radio-clear-pcm r)
      ;; what the worker sent and nobody will play is finished with too
      (incf (rbox-taken (radio-out r)) (count :pcm (%take (radio-out r)) :key #'first))
      (%post (radio-ctl r) (list :rejoin))
      (setf (radio-buffering-since r) (get-internal-real-time)
            (player-state p) :buffering))))

;;; ---- the worker ---------------------------------------------------------------
;;;
;;; Everything below runs on the station's own thread, on a WORKER struct of its own.

(defstruct (worker (:constructor %make-worker))
  url ctl out
  (rate 48000) (frame-samples 960) (gain 1.0d0)
  (stop nil)
  (stream nil)
  (metaint nil)
  (win (make-array *radio-window-size* :element-type '(unsigned-byte 8))
   :type (simple-array (unsigned-byte 8) (*)))
  (reed nil)
  (aligned nil)
  (name nil)
  (connects 0 :type fixnum)
  (bytes-in 0 :type integer)
  (posted 0 :type fixnum))              ; frames sent to the player (see RBOX-TAKEN)

;; %RADIO-OPEN (below, shared with nothing) records the open stream here, so a stop can close it.
(defun (setf radio-stream) (v w) (setf (worker-stream w) v))

(defun %w-fresh-reed (w)
  "A new reed player over W's window, with the window empty."
  (let* ((rp (reed:make-mp3-player (worker-win w)
                                   :rate (worker-rate w)
                                   :frame-samples (worker-frame-samples w)
                                   :gain (worker-gain w)))
         (d (reed::player-decoder rp)))
    (setf (reed::decoder-pos d) 0 (reed::decoder-end d) 0
          (worker-reed w) rp (worker-aligned w) nil)
    rp))

(defun %w-ahead (w)
  (let ((d (reed::player-decoder (worker-reed w))))
    (- (reed::decoder-end d) (reed::decoder-pos d))))

;; the alignment helpers read the window and the decoder through these
(defun radio-reed (w) (worker-reed w))
(defun radio-win (w) (worker-win w))

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


(defun %w-control (w)
  "The player's messages: (:STOP) ends the worker, (:REJOIN) starts the decoder afresh."
  (dolist (m (%take (worker-ctl w)))
    (case (first m)
      (:stop (setf (worker-stop w) t)
             (let ((s (worker-stream w))) (when s (ignore-errors (close s :abort t)))))
      (:rejoin (%w-fresh-reed w)))))

(defun %w-decode (w)
  "Decode every whole frame in hand and send each to the player."
  (loop
    (unless (worker-aligned w)
      (if (and (>= (%w-ahead w) *radio-align-ahead*) (%radio-align w))
          (setf (worker-aligned w) t)
          (return)))
    (when (< (%w-ahead w) *radio-decode-margin*) (return))
    (let* ((t0 (get-internal-real-time))
           (f (handler-case (reed:player-next-frame (worker-reed w))
                ;; a damaged frame (a dropped span, a false sync) can make the decoder fault
                ;; rather than refuse; it has stepped past it, so one frame is the whole cost
                (error () :bad)))
           ;; how long it took, in microseconds, for the player's stats
           (us (floor (* 1000000 (- (get-internal-real-time) t0)) internal-time-units-per-second)))
      (cond ((eq f :bad))
            (f (%post (worker-out w) (list :pcm (%shared-copy f) us))
               (incf (worker-posted w)))
            (t (%w-fresh-reed w) (return))))))   ; garbage, not a real end: start over

(defun %w-feed (w buf n)
  "Append N bytes of BUF to the window, compacting it first, and decode what is whole."
  (declare (type (simple-array (unsigned-byte 8) (*)) buf) (type fixnum n))
  (let* ((d (reed::player-decoder (worker-reed w)))
         (win (worker-win w))
         (pos (reed::decoder-pos d)) (end (reed::decoder-end d)))
    (when (> (+ end n) (length win))
      (replace win win :start2 pos :end2 end)
      (setf end (- end pos) pos 0))
    (when (> (+ end n) (length win))        ; still no room: drop the oldest
      (setf pos 0 end 0))
    (replace win buf :start1 end :end2 n)
    (setf (reed::decoder-pos d) pos (reed::decoder-end d) (+ end n))
    (incf (worker-bytes-in w) n))
  (%w-decode w))

(defun %w-pump (w stream metaint)
  "Read audio until the stream ends or the worker is stopped."
  (let ((buf (make-array 2048 :element-type '(unsigned-byte 8)))
        (meta (make-array 4096 :element-type '(unsigned-byte 8)))
        (left (or metaint most-positive-fixnum)))
    (loop until (progn (%w-control w) (worker-stop w))
          do ;; BACK-PRESSURE: while the player has nearly a full queue waiting, read nothing, and let
             ;; TCP hold the server back.  An Icecast server sends many seconds at once on connect
             ;; (SomaFM: 16 s in the first 30); decoded as fast as it came, it overflowed the queue,
             ;; whose oldest frames were dropped -- cut out of what was playing, thirty times a second.
             (loop while (and (not (worker-stop w))
                              (>= (- (worker-posted w) (rbox-taken (worker-out w)))
                                  (- *radio-max-frames* 25)))
                   do (sleep 0.02) (%w-control w))
             (let ((n (min 2048 left)))
               (when (plusp n)
                 (let ((got (read-sequence buf stream :end n)))
                   (when (zerop got) (return))
                   (%w-feed w buf got)
                   (decf left got)
                   (when (< got n) (return))))
               (when (zerop left)
                 (let ((lb (read-byte stream nil nil)))
                   (unless lb (return))
                   (let ((len (* 16 lb)))
                     (when (plusp len)
                       (unless (%radio-read-exactly stream meta len) (return))
                       (let ((title (%radio-parse-title (subseq meta 0 len))))
                         (when title (%post (worker-out w) (list :title title)))))))
                 (setf left metaint))))))

(defun %w-sleep (w seconds)
  (loop repeat (round (* seconds 10))
        until (progn (%w-control w) (worker-stop w))
        do (sleep 0.1)))

(defun %radio-worker (url rate frame-samples gain ctl out)
  "The station's thread: connect, read, decode, reconnect after a drop, until told to stop.
   Its arguments are plain data and the two boxes -- nothing of the player's."
  (let ((w (%make-worker :url url :ctl ctl :out out :rate rate
                         :frame-samples frame-samples :gain gain))
        (backoff 1) (failures 0))
    (%w-fresh-reed w)
    (handler-case
        (loop until (progn (%w-control w) (worker-stop w))
              do (let ((streamed nil))
                   (handler-case
                       (multiple-value-bind (stream headers) (%radio-open w url)
                         (unwind-protect
                              (let ((mi (ignore-errors
                                         (parse-integer (or (weft.fetch::get-header headers "icy-metaint") "")
                                                        :junk-allowed t))))
                                (incf (worker-connects w))
                                (setf (worker-metaint w) mi)
                                (let ((name (%radio-header-name headers "icy-name"))
                                      (genre (%radio-header-name headers "icy-genre")))
                                  (when name (%post out (list :name name)))
                                  (when genre (%post out (list :genre genre))))
                                (let ((before (worker-bytes-in w)))
                                  (%w-pump w stream (and mi (plusp mi) mi))
                                  (setf streamed (> (worker-bytes-in w) (+ before 8192)))))
                           (ignore-errors (close stream))
                           (setf (worker-stream w) nil)))
                     (radio-fatal (c)
                       (%post out (list :error (radio-fatal-why c)))
                       (setf (worker-stop w) t))
                     (serious-condition (c)
                       (unless (worker-stop w)
                         (incf failures)
                         (when (and (zerop (worker-connects w)) (>= failures *radio-max-attempts*))
                           (%post out (list :error (princ-to-string c)))
                           (setf (worker-stop w) t)))))
                   (unless (worker-stop w)
                     (if streamed
                         (setf backoff 1 failures 0)
                         (setf backoff (min 15 (* 2 backoff))))
                     (%w-sleep w backoff))))
      ;; nothing on this thread may end the image: a desktop runs under --disable-debugger
      (serious-condition (c) (ignore-errors (%post out (list :error (princ-to-string c))))))))

;;; ---- the player-facing side ---------------------------------------------------

(defun %radio-stop (p)
  "Stop and forget P's radio, if it has one.  Returns at once: the worker is told, closes its
   own connection and ends; nothing here waits for it."
  (let ((r (sb-thread:with-mutex ((player-lock p))
             (prog1 (player-radio p)
               (setf (player-radio p) nil (player-reed p) nil)))))
    (when r (%post (radio-ctl r) (list :stop)))
    r))

;;; The station's worker holds a 256 KB window and a decoder: a small heap is plenty, and on a phone
;;; whose world gives page workers 64 MB regions it should not take one of those.  Where modus lays its
;;; regions out by size (SB-THREAD:MAKE-THREAD :HEAP-BYTES) it asks for 16 MB; elsewhere -- SBCL, an
;;; older modus -- there is no such argument and nothing is passed.
(defparameter *radio-heap-bytes* (* 16 1024 1024))
(defun %radio-heap-args ()
  #+modus (and (find-symbol "*%THREAD-HEAP-WANT*" "MODUS-INT") (list :heap-bytes *radio-heap-bytes*))
  #-modus nil)

(defun play-url (p url &key name)
  "Play the live MP3 stream at URL on P.  Returns at once: the connection is made on the
station's own thread and the state is :buffering until enough audio has arrived, then
:playing.  A refusal (404, a codec we have no decoder for) arrives later as state
:error with the reason in PLAYER-ERROR — there is nothing to return it through now."
  (%radio-stop p)
  (let ((r (%make-radio :url url :name name :player p :ctl (%make-box) :out (%make-box))))
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
            (player-base p) 0.0d0
            (radio-buffering-since r) (get-internal-real-time)))
    (setf (radio-thread r)
          (apply #'sb-thread:make-thread #'%radio-worker
                 :arguments (list (%share url) (player-rate p) (player-frame-samples p)
                                  (player-gain p) (radio-ctl r) (radio-out r))
                 :name "spool-radio"
                 (%radio-heap-args)))
    url))

(defun player-live-p (p) (and (player-radio p) t))

(defun player-stream-title (p)
  "What the station says is playing (ICY StreamTitle), or NIL."
  (let ((r (player-radio p))) (and r (radio-title r))))

(defun player-station (p)
  (let ((r (player-radio p))) (and r (radio-name r))))
