;;;; play.lisp — an episode, playing.
;;;;
;;;; The whole output of this file is a SOURCE THUNK: no arguments, returns the next
;;;; frame of mono samples, or NIL.  That is reed's contract, which is glass's mixer's
;;;; contract, which is webrtc-media's :source contract.  Because all three are the
;;;; same shape, a playing episode can be handed to a mixer, an RTP sender or a file
;;;; writer without an adapter, and nothing in here has heard of any of them.
;;;;
;;;; PAUSE IS NIL.  A paused player's thunk returns NIL, and NIL in that contract
;;;; means "nothing right now, ask again" — the mixer fills the slot with silence and
;;;; its clock never stops.  So pause needs no cooperation from the consumer, no
;;;; unsubscribe/resubscribe dance, and no state anywhere but here.  The same is true
;;;; while a seek is in flight.
;;;;
;;;; Position is counted from samples HANDED OUT, not from where the decoder is.  The
;;;; decoder runs ahead by whatever the resampler is holding, and a progress bar that
;;;; tracks the decoder jitters forward of the sound.  What the listener means by
;;;; "where am I" is what they have heard.
;;;;
;;;; Seeking is by byte estimate, not by index, because MP3 has no index.  Position
;;;; maps to a byte offset by proportion of the file, the decoder resyncs at the next
;;;; frame header, and for a constant-bitrate file that lands within a frame.  For a
;;;; variable-bitrate one it lands approximately, which is the honest ceiling without
;;;; parsing a Xing TOC — noted here so the imprecision reads as a decision rather
;;;; than a bug.  A seek that lands early is corrected by the position bookkeeping:
;;;; POSITION is set to what was ASKED for, so the UI and the audio agree even when
;;;; the byte estimate was off.

(in-package #:spool)

(defparameter *play-rate* 48000
  "Native mix rate.  Decode once at the rate the mixer runs at and let each listener
convert down; resampling to 8 kHz here would make the whole desktop sound like a
phone call to every listener, which is the mistake glass's mixer exists to avoid.")

(defparameter *play-frame-samples* 960 "20 ms at 48 kHz — one mixer tick.")

(defstruct (player (:constructor %make-player))
  (episode nil)
  (path nil)                            ; the cached file
  (bytes nil)                           ; the file, held so a seek needn't re-read it
  (reed nil)                            ; the reed player, or NIL between seeks
  (state :idle)                         ; :idle :loading :playing :paused :ended :error
  (rate *play-rate* :type fixnum)
  (frame-samples *play-frame-samples* :type fixnum)
  (gain 1.0d0 :type double-float)
  (emitted 0 :type integer)             ; samples handed out since the last seek
  (base 0.0d0 :type double-float)       ; seconds the last seek jumped to
  (duration nil)
  (error nil)
  (on-end nil)                          ; called with the player when an episode ends
  (lock (sb-thread:make-mutex :name "spool-player")))

(defun player-position (p)
  "Seconds of audio the listener has actually heard."
  (+ (player-base p) (/ (player-emitted p) (float (player-rate p) 1d0))))

(defun %file-length (path)
  (with-open-file (s path :element-type '(unsigned-byte 8)) (file-length s)))

(defun %load-bytes (path)
  (with-open-file (s path :element-type '(unsigned-byte 8))
    (let ((b (make-array (file-length s) :element-type '(unsigned-byte 8))))
      (read-sequence b s) b)))

(defun make-player (&key (rate *play-rate*) (frame-samples *play-frame-samples*)
                         (gain 1.0d0) on-end)
  "An idle player.  Its SOURCE can be handed to a mixer immediately and will return
NIL — silence — until something is playing, so the wiring is done once at startup
and never touched again."
  (%make-player :rate rate :frame-samples frame-samples :gain (float gain 1d0)
                :on-end on-end))

(defun %open-reed (p &key (start 0))
  (setf (player-reed p)
        (reed:make-mp3-player (player-bytes p)
                              :rate (player-rate p)
                              :frame-samples (player-frame-samples p)
                              :gain (player-gain p)
                              :start start)))

(defun play (p episode &key (from nil) (lib (library)))
  "Play EPISODE on P, resuming from the remembered position unless FROM says where.

The audio must already be cached — this does not download, because downloading is
minutes long and this call is expected to return.  Callers use DOWNLOAD-ASYNC and
call PLAY from its completion, which is also what makes the UI's 'Downloading…'
state honest."
  (let ((path (cached-p episode)))
    (unless path
      (setf (player-state p) :error
            (player-error p) "not downloaded")
      (return-from play nil))
    (sb-thread:with-mutex ((player-lock p))
      (setf (player-state p) :loading
            (player-error p) nil
            (player-episode p) episode
            (player-path p) path
            (player-bytes p) (%load-bytes path)
            ;; The feed's advertised duration is a claim, and a wrong one makes the
            ;; seek arithmetic wrong.  Prefer it (it is usually right and is known
            ;; before any decoding), but a missing one is left NIL rather than
            ;; guessed — the UI can show "--:--", and seek refuses without it.
            (player-duration p) (feed:episode-seconds episode)
            (player-emitted p) 0
            (player-base p) 0.0d0)
      (let ((at (or from (position-of episode lib))))
        (if (and at (plusp at) (player-duration p) (< at (- (player-duration p) 5)))
            (%seek-locked p at)
            (%open-reed p)))
      (setf (player-state p) :playing))
    (player-episode p)))

(defun pause (p)
  (sb-thread:with-mutex ((player-lock p))
    (when (eq (player-state p) :playing) (setf (player-state p) :paused)))
  ;; Persist on pause, not on the timer: this is the moment the user told us they
  ;; are stopping, and it is cheap because it happens once.
  (when (player-episode p)
    (mark-position (player-episode p) (player-position p) :save t))
  (player-state p))

(defun resume (p)
  (sb-thread:with-mutex ((player-lock p))
    (when (member (player-state p) '(:paused :idle))
      (when (player-reed p) (setf (player-state p) :playing))))
  (player-state p))

(defun toggle (p)
  (if (eq (player-state p) :playing) (pause p) (resume p)))

(defun stop (p)
  (when (player-episode p)
    (ignore-errors (mark-position (player-episode p) (player-position p) :save t)))
  (sb-thread:with-mutex ((player-lock p))
    (setf (player-state p) :idle (player-reed p) nil (player-bytes p) nil
          (player-episode p) nil (player-path p) nil
          (player-emitted p) 0 (player-base p) 0.0d0))
  :idle)

(defun %seek-locked (p seconds)
  "Caller holds the lock.  Re-open the decoder at the byte offset SECONDS maps to."
  (let* ((dur (player-duration p))
         (len (length (player-bytes p)))
         (target (max 0.0d0 (min (float (or seconds 0) 1d0)
                                 (if dur (- (float dur 1d0) 0.5d0) most-positive-double-float))))
         (frac (if (and dur (plusp dur)) (/ target (float dur 1d0)) 0d0))
         ;; Land slightly EARLY on purpose.  Overshooting cuts off audio the listener
         ;; asked to hear; undershooting repeats a moment of it, which nobody notices.
         (off (max 0 (min (- len 4) (floor (* frac len 0.995))))))
    (setf (player-emitted p) 0
          (player-base p) target)
    (%open-reed p :start off)
    target))

(defun seek (p seconds)
  "Jump to SECONDS.  Needs a known duration; without one there is nothing to
compute a byte offset from, and inventing a duration would scrub to a random place."
  (when (and (player-reed p) (player-duration p))
    (sb-thread:with-mutex ((player-lock p))
      (%seek-locked p seconds))))

(defun skip (p delta)
  "Seek DELTA seconds from where we are — the ±30 s a listener actually presses."
  (seek p (+ (player-position p) delta)))

(defun next-episode (p &key (lib (library)))
  "The episode after the current one in its own feed — what plays when this ends."
  (let ((e (player-episode p)))
    (when e
      (let* ((eps (episodes (feed:episode-feed-url e) lib))
             (at (position (feed:episode-guid e) eps :key #'feed:episode-guid :test #'string=)))
        ;; Feeds are newest-first, so the NEXT one to listen to is the one BEFORE
        ;; this in the list — going the other way walks backwards through history.
        (when (and at (plusp at)) (nth (1- at) eps))))))

(defun player-source (p)
  "P as a bare source thunk.  Hand this to a mixer once; it stays valid across
episodes, pauses and seeks, because everything that changes is behind the closure."
  (lambda ()
    (handler-case
        (sb-thread:with-mutex ((player-lock p))
          (when (and (eq (player-state p) :playing) (player-reed p))
            (let ((f (reed:player-next-frame (player-reed p))))
              (cond
                (f (incf (player-emitted p) (length f)) f)
                (t ;; End of file.  Remember it as finished and tell whoever asked to
                   ;; be told — auto-advance is the caller's policy, not ours.
                   (setf (player-state p) :ended)
                   (let ((e (player-episode p)))
                     (when e (ignore-errors (mark-played e))))
                   (when (player-on-end p)
                     ;; Off this thread: ON-END may start the next episode, which
                     ;; reads a 60 MB file, and this thunk is called from the mixer's
                     ;; 20 ms clock.  Blocking it stalls every listener at once.
                     (let ((cb (player-on-end p)))
                       (sb-thread:make-thread (lambda () (ignore-errors (funcall cb p)))
                                              :name "spool-on-end")))
                   nil)))))
      (serious-condition (e)
        ;; A decode error is one silent frame and a state change, never a thrown
        ;; condition: this runs on the mixer's thread, and the mixer runs under
        ;; --disable-debugger, where an escaping condition takes the whole image.
        (setf (player-state p) :error (player-error p) (princ-to-string e))
        nil))))

(defun player-report (p)
  "One line, for a control socket or a status bar."
  (let ((e (player-episode p)))
    (format nil "~(~a~)~@[ ~a~] ~a/~a~@[ err=~a~]"
            (player-state p)
            (and e (feed:episode-title e))
            (feed::format-duration (player-position p))
            (feed::format-duration (player-duration p))
            (player-error p))))
