;;;; app.lisp — the podcast client as a window on the glass desktop.
;;;;
;;;; This is built for a specific listener in a specific posture: someone holding a
;;;; PHONE, driving the glass desktop over WebRTC, with the sound coming out of the
;;;; session mixer rather than out of the phone.  That posture decides most of what
;;;; follows.
;;;;
;;;; TOUCH TARGETS, NOT TEXT.  Every clickable thing is a filled rectangle at least
;;;; a fingertip tall, and the whole row is the presentation — not just the words in
;;;; it.  A remote pointer over a scaled framebuffer is a blunt instrument, and a
;;;; hit area the size of a line of type cannot be aimed at.
;;;;
;;;; PAGES, NOT SCROLLBARS.  A feed here has 500 episodes.  Dragging a scrollbar
;;;; through a lossy video stream is miserable, and a pane that redisplays on a timer
;;;; loses its scroll position anyway — so the episode list is paged, with two big
;;;; buttons, and the timer never fights the user for position.
;;;;
;;;; NOTHING SLOW RUNS IN THE FRAME'S THREAD.  Subscribing, refreshing and downloading
;;;; all happen on their own threads with a status line, because a window that stops
;;;; painting for eight seconds is indistinguishable, over a remote framebuffer, from
;;;; a window that has crashed.  The commands here are all short.
;;;;
;;;; THE TICK IS CONDITIONAL.  A thread nudges the frame once a second WHILE something
;;;; is actually moving (playing, downloading, refreshing) and otherwise leaves it
;;;; alone: an idle podcast client has nothing new to say, and redrawing it anyway
;;;; would spend the desktop's frame budget to display the same pixels.
;;;;
;;;; CLOSING THE WINDOW DOES NOT STOP THE SHOW.  The audio belongs to the session's
;;;; mixer, not to this frame — you can close the client and keep listening, which is
;;;; what "the box is playing a podcast" ought to mean.

(defpackage #:spool.app
  (:use #:cl)
  (:local-nicknames (#:feed #:spool.feed) (#:sg #:spool.glass))
  (:export #:podcasts #:register #:run))

(in-package #:spool.app)

;;; ---- look ------------------------------------------------------------------

(defparameter *row-height* 30 "Tall enough to hit with a thumb over a scaled framebuffer.")
(defparameter *page-size* 12 "Episodes per page before the pane has been measured.")
(defparameter *transport-height* 122
  "The transport's height, fixed by the background it paints.  A McCLIM stream pane
takes its minimum height from its CONTENT, not from the :HEIGHT it was made with,
so a pane that wants a definite height has to draw one.")

(defun ui-font (&optional (size 13)) (clim:make-text-style :sans-serif :roman size))
(defun ui-bold (&optional (size 13)) (clim:make-text-style :sans-serif :bold size))

(defparameter +row-bg+      (clim:make-rgb-color 0.96 0.96 0.94))
(defparameter +row-alt+     (clim:make-rgb-color 0.91 0.91 0.89))
(defparameter +row-sel+     (clim:make-rgb-color 0.78 0.84 0.95))
(defparameter +btn-bg+      (clim:make-rgb-color 0.85 0.85 0.83))
(defparameter +btn-hot+     (clim:make-rgb-color 0.70 0.80 0.70))
(defparameter +dim+         (clim:make-rgb-color 0.40 0.40 0.40))
(defparameter +bar-track+   (clim:make-rgb-color 0.75 0.75 0.73))
(defparameter +bar-fill+    (clim:make-rgb-color 0.20 0.45 0.75))

(defun %fit (stream text width)
  "TEXT shortened until it fits WIDTH, with an ellipsis.  Measured rather than
counted, because the desktop's font is proportional and a character budget either
wastes half the row or overruns it."
  (let ((text (or text "")))
    (if (<= (clim:text-size stream text) width)
        text
        (loop for n downfrom (length text) above 1
              for s = (concatenate 'string (subseq text 0 n) "...")
              when (<= (clim:text-size stream s) width) return s
              finally (return "...")))))

;;; ---- presentation types ----------------------------------------------------
;;; Distinct types rather than raw structs/numbers so a click on a row cannot be
;;; read as a click on a seek point, and so each has its own gesture.

(clim:define-presentation-type spool-feed ())
(clim:define-presentation-type spool-episode ())
(clim:define-presentation-type transport-button ())
(clim:define-presentation-type seek-point ())

;;; Every one of these needs a PRESENT method, and not for looks: when a click
;;; translates to a command, the frame ECHOES the command's arguments into the
;;; interactor, and echoing means PRESENT.  Without a method the default prints the
;;; object — and a feed here is a struct holding 355 episodes, each with a summary
;;; paragraph, so selecting a show spent twenty-one seconds rendering a megabyte of
;;; struct into a two-line pane.  A presentation type that is clickable owes the
;;; screen a short name for the thing it is showing.

(clim:define-presentation-method clim:present
    (f (type spool-feed) stream (view t) &key)
  (write-string (or (feed:feed-title f) (feed:feed-url f) "?") stream))

(clim:define-presentation-method clim:present
    (ep (type spool-episode) stream (view t) &key)
  (write-string (or (feed:episode-title ep) "?") stream))

(clim:define-presentation-method clim:present
    (b (type transport-button) stream (view t) &key)
  (format stream "~(~a~)" b))

(clim:define-presentation-method clim:present
    (secs (type seek-point) stream (view t) &key)
  (write-string (feed:format-duration secs) stream))

;;; ---- the frame -------------------------------------------------------------

(clim:define-application-frame podcasts ()
  ((player :initform nil :accessor app-player)
   (feed :initform nil :accessor app-feed :documentation "The feed being listed.")
   (page :initform 0 :accessor app-page)
   (status :initform "" :accessor app-status)
   (log :initform '() :accessor app-log
        :documentation "Recent notes, newest first — see %NOTE.")
   (busy :initform 0 :accessor app-busy :documentation "Background jobs in flight.")
   (ticker :initform nil :accessor app-ticker)
   (rows :initform *page-size* :accessor app-rows
         :documentation "Episode rows per page, measured from the pane at draw time."))
  (:menu-bar nil)
  (:pointer-documentation t)
  (:panes
   (transport :application :display-function 'draw-transport
              :scroll-bars nil :height *transport-height* :text-style (ui-font))
   (feeds :application :display-function 'draw-feeds
          :scroll-bars :vertical :width 260 :text-style (ui-font))
   (episodes :application :display-function 'draw-episodes
             :scroll-bars nil :text-style (ui-font))
   (interactor :interactor :height 72 :text-style (ui-font 12)))
  (:layouts
   (default
    (clim:vertically ()
      transport
      (:fill (clim:horizontally () feeds (:fill episodes)))
      interactor))))

(defun %player (frame)
  "The session player, attached to the box's mixer on first use.  ATTACH is
idempotent, so this is safe to call from any display function."
  (or (app-player frame) (setf (app-player frame) (sg:attach))))

(defun %say (frame fmt &rest args)
  (setf (app-status frame) (apply #'format nil fmt args)))

(defparameter *log-size* 8 "How many notes are kept for the user to look back at.")

(defun %note (frame fmt &rest args)
  "Say it in the status line AND keep it, so it can still be read afterwards.

The status line is ONE line that the very next action overwrites, and that is where
the only report of a failed subscribe used to go: a URL 404s, the line says so, the
user taps Refresh, and now nothing anywhere says why the show never appeared — the
app looks like it silently did nothing.  Anything a user would ask \"what happened?\"
about goes through here.

The kept copy is drawn in the episode pane (see DRAW-EPISODES), not written to the
interactor: the interactor belongs to the command loop, which leaves a prompt on the
open line, and text arriving there from a background thread overstrikes it.  Called
from background threads, so this must not signal — an escaping condition under
--disable-debugger takes the image with it."
  (let ((text (apply #'format nil fmt args)))
    (setf (app-status frame) text
          (app-log frame) (cons text (subseq (app-log frame)
                                             0 (min (length (app-log frame))
                                                    (1- *log-size*)))))
    text))

(defun %ask (frame prompt)
  "Ask for one line in the interactor and return it, or NIL if the user backed out.

This is a nested ACCEPT — the same thing the command loop does for a command's
unsupplied argument — and it BLOCKS the frame until it is answered.  That is the
whole risk of it: a prompt with no way out is a window that has stopped answering
its pointer, which from a phone is indistinguishable from a crash.  So every exit
lands here: Enter on an empty line, the abort gesture, and any condition the
accept itself signals."
  (let ((s (clim:find-pane-named frame 'interactor)))
    (%say frame "~a — type it below and press Enter (empty cancels)." prompt)
    (clim:redisplay-frame-pane frame 'transport :force-p t)
    (unwind-protect
         (handler-case
             (let ((v (clim:accept 'string :stream s :prompt prompt :default ""
                                   :view clim:+textual-view+)))
               (let ((v (and (stringp v) (string-trim '(#\Space #\Tab) v))))
                 (and v (plusp (length v)) v)))
           (clim:abort-gesture () nil)
           (serious-condition () nil))
      (%say frame ""))))

(defmacro %background ((frame what) &body body)
  "Run BODY on its own thread, counting it as busy and reporting failure in the
status line instead of taking the image down (this desktop runs under
--disable-debugger, where an escaping condition in any thread is fatal)."
  (let ((f (gensym)) (w (gensym)))
    `(let ((,f ,frame) (,w ,what))
       (incf (app-busy ,f))
       (%say ,f "~a..." ,w)
       (sb-thread:make-thread
        (lambda ()
          (unwind-protect
               (handler-case (progn ,@body)
                 (serious-condition (e) (%note ,f "~a failed: ~a" ,w e)))
            (decf (app-busy ,f))))
        :name (format nil "spool-app ~a" ,w)))))

;;; ---- transport -------------------------------------------------------------

(defun %button (stream label object &key (width 92) (height 34) hot)
  "A labelled rectangle that is one presentation, drawn at the cursor."
  (multiple-value-bind (x y) (clim:stream-cursor-position stream)
    (clim:with-output-as-presentation (stream object 'transport-button)
      (clim:draw-rectangle* stream x y (+ x width) (+ y height)
                            :ink (if hot +btn-hot+ +btn-bg+))
      (clim:draw-rectangle* stream x y (+ x width) (+ y height) :ink +dim+ :filled nil)
      (let ((tw (clim:text-size stream label)))
        (clim:draw-text* stream label (+ x (/ (- width tw) 2)) (+ y (/ height 2))
                         :align-y :center :text-style (ui-bold 13))))
    (setf (clim:stream-cursor-position stream) (values (+ x width 8) y))))

(defun draw-transport (frame stream)
  (let* ((p (%player frame))
         (ep (spool:player-episode p))
         (state (spool:player-state p))
         (pos (spool:player-position p))
         (dur (spool:player-duration p))
         (width (max 320 (clim:bounding-rectangle-width (clim:sheet-region stream)))))
    ;; The wash that sets this pane's height (see *TRANSPORT-HEIGHT*).  Without it the
    ;; pane ends exactly at the last thing drawn, and the status line's descenders are
    ;; sliced off by the pane border.
    (clim:draw-rectangle* stream 0 0 width *transport-height* :ink +row-bg+)
    ;; Now playing.
    (clim:draw-text* stream (%fit stream (if ep (feed:episode-title ep) "Nothing playing")
                                  (- width 24))
                     10 18 :text-style (ui-bold 14))
    ;; The bar: a track, the played part, and 80 invisible slices over the top so a
    ;; tap anywhere on it seeks there.  Slices rather than one wide presentation
    ;; because a presentation carries a fixed object, and "where did they touch" has
    ;; to be baked into the object at draw time.
    (let* ((bx 10) (by 30) (bw (- width 20)) (bh 14)
           (frac (if (and dur (plusp dur)) (min 1d0 (/ pos dur)) 0d0)))
      (clim:draw-rectangle* stream bx by (+ bx bw) (+ by bh) :ink +bar-track+)
      (when (plusp frac)
        (clim:draw-rectangle* stream bx by (+ bx (* bw frac)) (+ by bh) :ink +bar-fill+))
      (when (and dur (plusp dur))
        (let ((n 80))
          (dotimes (i n)
            (let ((x0 (+ bx (* bw (/ i n)))) (x1 (+ bx (* bw (/ (1+ i) n)))))
              (clim:with-output-as-presentation (stream (* dur (/ (+ i 0.5) n)) 'seek-point)
                ;; Drawn in the track's own colour: invisible, but a real output
                ;; record, which is what makes it clickable.
                (clim:draw-rectangle* stream x0 by x1 (+ by bh)
                                      :ink (if (< (/ (+ i 0.5) n) frac) +bar-fill+ +bar-track+))))))))
    (clim:draw-text* stream (format nil "~a / ~a" (feed:format-duration pos)
                                    (feed:format-duration dur))
                     10 58 :text-style (ui-font 12) :ink +dim+)
    ;; Buttons.
    (setf (clim:stream-cursor-position stream) (values 10 68))
    (%button stream "<< 30" :back)
    (%button stream (if (eq state :playing) "Pause" "Play") :toggle
             :hot (eq state :playing))
    (%button stream "30 >>" :fwd)
    (%button stream "Next" :next)
    (%button stream "Refresh" :refresh)
    ;; Status: the one line that explains anything slow or anything that failed.
    (let ((msg (app-status frame)))
      (when (plusp (length msg))
        (clim:draw-text* stream (%fit stream msg (- width 24)) 10 112
                         :text-style (ui-font 12) :ink +dim+)))))

;;; ---- feeds -----------------------------------------------------------------

(defun draw-feeds (frame stream)
  (let ((width (max 200 (clim:bounding-rectangle-width (clim:sheet-region stream))))
        (y 0)
        (current (app-feed frame)))
    (clim:draw-text* stream "SHOWS" 8 16 :text-style (ui-bold 12) :ink +dim+)
    (incf y 24)
    (dolist (f (spool:subscriptions))
      (let ((sel (and current (string= (feed:feed-url f) (feed:feed-url current)))))
        (clim:with-output-as-presentation (stream f 'spool-feed)
          (clim:draw-rectangle* stream 0 y width (+ y *row-height*)
                                :ink (if sel +row-sel+ +row-bg+))
          (clim:draw-text* stream (%fit stream (feed:feed-title f) (- width 46))
                           8 (+ y 14) :text-style (if sel (ui-bold) (ui-font)))
          (clim:draw-text* stream (format nil "~d" (length (feed:feed-episodes f)))
                           (- width 8) (+ y 14) :align-x :right
                           :text-style (ui-font 11) :ink +dim+)))
      (incf y (1+ *row-height*)))
    (setf (clim:stream-cursor-position stream) (values 4 (+ y 8)))
    (%button stream "Add show" :add :width 110 :height 30)))

;;; ---- episodes --------------------------------------------------------------

(defun %episode-state (frame ep)
  "What this episode is doing, as one keyword — the only thing the icon shows."
  (let ((p (app-player frame)))
    (cond ((and p (spool:player-episode p)
                (string= (feed:episode-guid ep)
                         (feed:episode-guid (spool:player-episode p))))
           (if (eq (spool:player-state p) :playing) :playing :paused))
          ((spool:downloading-p ep) :downloading)
          ((spool:cached-p ep) :cached)
          ((spool:played-p ep) :played)
          (t :remote))))

(defun %draw-state-icon (stream state cx cy)
  "State as a SHAPE, not a glyph: the desktop's font may not have a triangle, and a
missing glyph in a status column reads as a bug rather than as a state."
  (let ((r 6))
    (ecase state
      (:playing (clim:draw-polygon* stream (list (- cx 4) (- cy 6) (- cx 4) (+ cy 6) (+ cx 6) cy)
                                    :ink +bar-fill+))
      (:paused (clim:draw-rectangle* stream (- cx 5) (- cy 6) (- cx 1) (+ cy 6) :ink +bar-fill+)
               (clim:draw-rectangle* stream (+ cx 1) (- cy 6) (+ cx 5) (+ cy 6) :ink +bar-fill+))
      (:downloading (clim:draw-circle* stream cx cy r :ink +bar-fill+ :filled nil)
                    (clim:draw-line* stream cx cy cx (+ cy r) :ink +bar-fill+))
      (:cached (clim:draw-circle* stream cx cy 5 :ink +dim+))
      (:played (clim:draw-line* stream (- cx 5) cy (+ cx 5) cy :ink +dim+))
      (:remote (clim:draw-circle* stream cx cy 5 :ink +dim+ :filled nil)))))

(defun %pub-date (ep)
  (let ((u (feed:episode-published ep)))
    (if (null u)
        ""
        (multiple-value-bind (s m h day month) (decode-universal-time u)
          (declare (ignore s m h))
          (format nil "~a ~d" (aref #("" "Jan" "Feb" "Mar" "Apr" "May" "Jun"
                                      "Jul" "Aug" "Sep" "Oct" "Nov" "Dec") month)
                  day)))))

(defun %page-episodes (frame)
  "(values episodes-on-this-page total page-count)."
  (let* ((all (if (app-feed frame) (feed:feed-episodes (app-feed frame)) '()))
         (n (length all))
         (per (app-rows frame))
         (pages (max 1 (ceiling n per)))
         (page (max 0 (min (app-page frame) (1- pages))))
         (start (* page per)))
    (values (subseq all start (min n (+ start per))) n pages)))

(defun %measure-rows (frame stream)
  "How many episode rows fit in STREAM, leaving room for the header and the pager.
Measured every draw rather than fixed, because this window is as tall as whatever
phone is looking at it — and a page whose Older button falls off the bottom edge is
a page you cannot leave.  One row is given up while there is something to report, so
the report is on screen whether or not a show happens to be selected."
  (let ((h (clim:bounding-rectangle-height (clim:sheet-region stream))))
    (max 1 (floor (- h 24 46 (if (app-log frame) 22 0)) (1+ *row-height*)))))

(defun draw-episodes (frame stream)
  (setf (app-rows frame) (%measure-rows frame stream))
  (let ((width (max 320 (clim:bounding-rectangle-width (clim:sheet-region stream)))))
    (if (null (app-feed frame))
        ;; Nothing is selected, so this pane is empty — room for the whole log, which is
        ;; also where you are already looking after tapping Add show.
        (progn
          (clim:draw-text* stream "Pick a show on the left." 10 20
                           :text-style (ui-font 13) :ink +dim+)
          (loop for line in (app-log frame)
                for y from 56 by 22
                do (clim:draw-text* stream (%fit stream line (- width 20)) 10 y
                                    :text-style (ui-font 12) :ink +dim+)))
        (multiple-value-bind (page total pages) (%page-episodes frame)
          (clim:draw-text* stream (%fit stream (feed:feed-title (app-feed frame)) (- width 160))
                           8 16 :text-style (ui-bold 13))
          (clim:draw-text* stream (format nil "~d episodes  ~d/~d" total (1+ (app-page frame)) pages)
                           (- width 8) 16 :align-x :right :text-style (ui-font 11) :ink +dim+)
          (let ((y 24) (i 0))
            (dolist (ep page)
              (let ((state (%episode-state frame ep)))
                (clim:with-output-as-presentation (stream ep 'spool-episode)
                  (clim:draw-rectangle* stream 0 y width (+ y *row-height*)
                                        :ink (if (member state '(:playing :paused))
                                                 +row-sel+
                                                 (if (evenp i) +row-bg+ +row-alt+)))
                  (%draw-state-icon stream state 16 (+ y (/ *row-height* 2)))
                  (clim:draw-text* stream (%fit stream (feed:episode-title ep) (- width 150))
                                   32 (+ y 14) :text-style (ui-font))
                  (clim:draw-text* stream (feed:format-duration (feed:episode-seconds ep))
                                   (- width 60) (+ y 14) :align-x :right
                                   :text-style (ui-font 11) :ink +dim+)
                  (clim:draw-text* stream (%pub-date ep) (- width 6) (+ y 14) :align-x :right
                                   :text-style (ui-font 11) :ink +dim+)))
              (incf y (1+ *row-height*))
              (incf i))
            (setf (clim:stream-cursor-position stream) (values 4 (+ y 10)))
            (%button stream "Newer" :newer)
            (%button stream "Older" :older)
            (%button stream "Refresh show" :refresh-feed :width 130)
            ;; The last thing worth reporting, under the pager, on the row %MEASURE-ROWS
            ;; gave up for it.  The status line above says the same thing until the next
            ;; tap rewrites it; this one stays.
            (let ((note (first (app-log frame))))
              (when note
                (clim:draw-text* stream (%fit stream note (- width 20)) 10 (+ y 66)
                                 :text-style (ui-font 12) :ink +dim+))))))))

;;; ---- commands --------------------------------------------------------------

(define-podcasts-command (com-show-feed) ((f 'spool-feed :gesture :select))
  (setf (app-feed clim:*application-frame*) f
        (app-page clim:*application-frame*) 0)
  (%say clim:*application-frame* ""))

(define-podcasts-command (com-play-episode) ((ep 'spool-episode :gesture :select))
  (let* ((frame clim:*application-frame*)
         (p (%player frame)))
    ;; Tapping the episode that is already playing is a pause, not a restart from
    ;; the remembered position — a listener who taps the highlighted row means
    ;; "stop talking", never "start this over".
    (if (and (spool:player-episode p)
             (string= (feed:episode-guid ep) (feed:episode-guid (spool:player-episode p))))
        (spool:toggle p)
        (case (sg:ensure-playing ep)
          (:playing (%say frame "Playing ~a" (feed:episode-title ep)))
          (:downloading (%say frame "Downloading ~,1f MB..."
                              (/ (or (feed:episode-bytes ep) 0) 1048576.0)))
          (t (%say frame "Not downloaded."))))))

(define-podcasts-command (com-seek-to) ((secs 'seek-point :gesture :select))
  (let ((p (%player clim:*application-frame*)))
    (if (spool:player-duration p)
        (spool:seek p secs)
        (%say clim:*application-frame* "No duration for this episode — cannot seek."))))

(define-podcasts-command (com-press) ((b 'transport-button :gesture :select))
  (let* ((frame clim:*application-frame*)
         (p (%player frame)))
    (case b
      (:toggle (spool:toggle p))
      (:back (spool:skip p -30))
      (:fwd (spool:skip p 30))
      (:next (let ((next (spool:next-episode p)))
               (cond ((null next) (%say frame "No next episode."))
                     (t (com-play-episode next)))))
      (:newer (setf (app-page frame) (max 0 (1- (app-page frame)))))
      (:older (multiple-value-bind (page total pages) (%page-episodes frame)
                (declare (ignore page total))
                (setf (app-page frame) (min (1- pages) (1+ (app-page frame))))))
      ;; The button used to point at a command the user should type, which is a
      ;; button that does nothing: on a phone the box below is not even in reach
      ;; until the soft keyboard is up.  It asks now.
      (:add (let ((url (%ask frame "feed URL")))
              (if url
                  (com-add-feed url)
                  (%say frame "Nothing added."))))
      (:refresh-feed (let ((f (app-feed frame)))
                       (when f
                         (%background (frame "Refreshing")
                           (let ((new (spool:refresh (feed:feed-url f))))
                             (setf (app-feed frame) new)
                             (%say frame "~a: ~d episodes"
                                   (feed:feed-title new) (length (feed:feed-episodes new))))))))
      (:refresh (%background (frame "Refreshing all")
                  (multiple-value-bind (ok bad) (spool:refresh-all)
                    (let ((f (app-feed frame)))
                      (when f (setf (app-feed frame)
                                    (find (feed:feed-url f) (spool:subscriptions)
                                          :key #'feed:feed-url :test #'string=))))
                    (%say frame "Refreshed ~d show~:p~[~:;, ~:*~d failed~]" ok (length bad))))))))

(define-podcasts-command (com-add-feed :name "Add Feed") ((url 'string :prompt "feed URL"))
  ;; Both outcomes go through %NOTE, because both are things you would ask about
  ;; later, and the failure names the URL BACK TO YOU.  A typo is the likeliest thing
  ;; to go wrong here and a phone keyboard makes it likelier still — "failed: HTTP
  ;; 404" alone doesn't let you see that you dropped a character off the end of it.
  (let ((frame clim:*application-frame*))
    (%background (frame "Subscribing")
      (handler-case
          (let ((f (spool:subscribe url)))
            (setf (app-feed frame) f (app-page frame) 0)
            (%note frame "Added ~a (~d episodes)" (feed:feed-title f)
                   (length (feed:feed-episodes f))))
        (serious-condition (e) (%note frame "Could not add ~a: ~a" url e))))))

(define-podcasts-command (com-remove-feed :name "Remove Show") ((f 'spool-feed :prompt "show"))
  (spool:unsubscribe (feed:feed-url f))
  (when (eq f (app-feed clim:*application-frame*))
    (setf (app-feed clim:*application-frame*) nil))
  (%say clim:*application-frame* "Removed ~a" (feed:feed-title f)))

(define-podcasts-command (com-download :name "Download") ((ep 'spool-episode :prompt "episode"))
  (let ((frame clim:*application-frame*))
    (if (spool:cached-p ep)
        (%say frame "Already downloaded.")
        (progn
          (incf (app-busy frame))
          (spool:download-async
           ep
           :on-done (lambda (path) (declare (ignore path))
                      (decf (app-busy frame))
                      (%say frame "Downloaded ~a" (feed:episode-title ep)))
           :on-error (lambda (e) (decf (app-busy frame))
                       (%say frame "Download failed: ~a" e)))
          (%say frame "Downloading ~a..." (feed:episode-title ep))))))

(define-podcasts-command (com-cache-report :name "Cache") ()
  (multiple-value-bind (bytes files) (spool:cache-size)
    (%say clim:*application-frame* "~,1f GB in ~d file~:p at ~a"
          (/ bytes 1073741824.0) files (spool:library-root))))

(define-podcasts-command (com-tick) ()
  ;; Nothing: the frame's top level redisplays the panes after every command, so
  ;; the tick's whole job is to BE a command.
  nil)

(define-podcasts-command (com-quit :name "Quit") ()
  ;; The mixer keeps the show.  Closing the window is closing a window.
  (clim:frame-exit clim:*application-frame*))

;;; ---- the tick --------------------------------------------------------------

(defun %moving-p (frame)
  (let ((p (app-player frame)))
    (or (plusp (app-busy frame))
        (and p (eq (spool:player-state p) :playing)))))

(defun %start-ticker (frame)
  "Nudge the frame once a second while something is moving.  EXECUTE-FRAME-COMMAND
from another thread APPENDS to the frame's event queue rather than running the
command here, which is what makes this safe: the redisplay still happens in the
frame's own process."
  (sb-thread:make-thread
   (lambda ()
     (loop
       (sleep 1)
       (unless (member (clim:frame-state frame) '(:enabled :shrunk)) (return))
       (when (%moving-p frame)
         (handler-case (clim:execute-frame-command frame '(com-tick))
           (serious-condition () (return))))))
   :name "spool-app-tick"))

(defmethod clim:run-frame-top-level :around ((frame podcasts) &key)
  (setf (app-ticker frame) (%start-ticker frame))
  (unwind-protect (call-next-method)
    ;; The thread also notices the frame is gone on its own; this just makes it
    ;; prompt rather than up-to-a-second late.
    (ignore-errors (sb-thread:terminate-thread (app-ticker frame)))))

;;; ---- launching -------------------------------------------------------------

(defun run (&key (width 980) (height 660))
  "Run the client standalone (its own top level), for testing outside the desktop."
  (clim:run-frame-top-level
   (clim:make-application-frame 'podcasts :width width :height height)))

(defun register (&key (label "Podcasts") (width 980) (height 660))
  "Put the client in the glass desktop's root menu.  Found by name so that loading
this system in an image without the glass backend is not an error — the app is
still usable through RUN."
  (let ((fn (and (find-package "CLIM-GLASS") (find-symbol "REGISTER-APP" "CLIM-GLASS"))))
    (when (and fn (fboundp fn))
      (funcall fn label (list 'podcasts :width width :height height))
      label)))
