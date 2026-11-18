;;;; desk.lisp — the podcast client as a plain glass window: no McCLIM.
;;;;
;;;; The same client as app.lisp (the CLIM frame), drawn by hand into the window's framebuffer
;;;; and driven through the window manager's MAKE-FN contract (glass.wm WM-OPEN; the model is
;;;; glass's voice-apps.lisp): PODCASTS-APP is called with the window's FB and returns ON-KEY
;;;; ON-POINTER DIRTY-P COPY-P CLOSE-FN RESIZE-FN TEXT-P.  The listener it is built for has not
;;;; changed, so neither have the rules — see app.lisp's header for the long form:
;;;;
;;;; TOUCH TARGETS, NOT TEXT.  Every tappable thing is a filled rectangle a fingertip tall, the
;;;; whole row is the target, and what can be hit is exactly what was drawn: every redraw records
;;;; its targets as it paints them (TARGET), and a press is looked up in that list.
;;;;
;;;; PAGES, NOT SCROLLBARS.  Episodes AND shows are paged with big buttons; how many rows fit is
;;;; measured from the window at every draw, so the pager is never off the bottom edge.  A narrow
;;;; window (a phone held upright) shows one list at a time — shows, then a show's episodes with
;;;; a "< Shows" button — instead of two columns that would each be too thin to read.
;;;;
;;;; NOTHING SLOW ON THE WINDOW MANAGER'S THREAD.  ON-POINTER, ON-KEY and DIRTY-P run on the
;;;; compositor's thread, and a window that stops painting there stops the whole desktop.
;;;; Subscribing, refreshing, downloading and measuring the cache all run on their own threads
;;;; and report through the status line (%NOTE keeps the ones you would ask about later).
;;;;
;;;; THE TICK IS CONDITIONAL.  DIRTY-P is polled by the window manager every frame; this window
;;;; looks at the world at most once a second, and repaints only when what it would show has
;;;; changed — the clock while playing, a download's progress while one is running.  An idle
;;;; client costs a comparison a second and no pixels.
;;;;
;;;; CLOSING THE WINDOW DOES NOT STOP THE SHOW.  The audio is the session mixer's (spool/glass).
;;;;
;;;; TYPING.  "Add show" needs a URL.  The window says TEXT-P NIL, because the window manager
;;;; raises the phone's keyboard on EVERY tap into a text window, and over an episode list that
;;;; is half the screen gone for nothing.  So adding a show is a mode of its own with an entry
;;;; line that takes keys (a hardware keyboard, a VNC viewer, or the session's paste-as-keys)
;;;; and a Paste button that reads the session clipboard — copy the URL on the phone, tap Paste.

(defpackage #:spool.desk
  (:use #:cl)
  (:local-nicknames (#:feed #:spool.feed) (#:sg #:spool.glass))
  (:export #:podcasts-app #:podcasts-window #:win-targets #:target-label #:target-x #:target-y
           #:target-w #:target-h #:win-feed #:win-page #:win-rows #:win-feed-page #:win-view
           #:win-mode #:win-entry #:win-status #:win-confirm #:*podcasts-app* #:*default-width* #:*default-height*))

(in-package #:spool.desk)

;;; ---- look ----------------------------------------------------------------------------------

(defparameter *default-width* 980)
(defparameter *default-height* 660)

(defparameter *row-height* 36 "Tall enough to hit with a thumb over a scaled framebuffer.")
(defparameter *button-height* 38)
(defparameter *transport-height* 134)
(defparameter *feeds-width* 260 "The shows column, in a window wide enough for two.")
(defparameter *narrow* 600 "Below this width the window shows one list at a time.")
(defparameter *log-size* 8 "How many notes are kept for the user to look back at.")

(defparameter +bg+        #xffffff)
(defparameter +ink+       #x1a1a1a)
(defparameter +dim+       #x666666)
(defparameter +rule+      #xbfbfba)
(defparameter +row-bg+    #xf5f5f0)
(defparameter +row-alt+   #xe8e8e3)
(defparameter +row-sel+   #xc7d6f2)
(defparameter +btn-bg+    #xd9d9d4)
(defparameter +btn-edge+  #x8c8c88)
(defparameter +btn-hot+   #xb3ccb3)
(defparameter +btn-warn+  #xf0b8a8)
(defparameter +bar-track+ #xbfbfba)
(defparameter +bar-fill+  #x3373bf)

(defun %font (bold) (glass:default-font bold))

(defun %width (text size bold) (glass:text-width text :size size :font (%font bold)))

(defun %text (fb x y text &key (size 15) bold (color +ink+))
  (glass:fb-text fb x y text :size size :color color :font (%font bold)))

(defun %fit (text width &key (size 15) bold)
  "TEXT shortened until it fits WIDTH, with an ellipsis.  Measured, because the font is
   proportional; a binary search, because a 300-character episode title measured one character
   at a time is 300 measurements per row per redraw."
  (let ((text (or text "")))
    (cond ((<= (%width text size bold) width) text)
          ((<= width (%width "..." size bold)) "")
          (t (let ((lo 0) (hi (length text)))     ; LO fits, HI does not
               (loop while (> (- hi lo) 1)
                     do (let ((mid (floor (+ lo hi) 2)))
                          (if (<= (%width (concatenate 'string (subseq text 0 mid) "...") size bold)
                                  width)
                              (setf lo mid)
                              (setf hi mid))))
               (concatenate 'string (string-right-trim " " (subseq text 0 lo)) "..."))))))

;;; ---- the window ----------------------------------------------------------------------------

(defclass target ()
  ((x :initarg :x :reader target-x) (y :initarg :y :reader target-y)
   (w :initarg :w :reader target-w) (h :initarg :h :reader target-h)
   (action :initarg :action :reader target-action
           :documentation "Called with the press's window-local X and Y.")
   (label :initarg :label :initform nil :reader target-label
          :documentation "A button's label: for a test to find it by, as a person would."))
  (:documentation "Somewhere a press does something: recorded as it is drawn, so the hit areas
   are always the ones on screen."))

(defclass podcasts-window ()
  ((fb :initarg :fb :reader win-fb)
   (feed :initform nil :accessor win-feed :documentation "The show being listed.")
   (page :initform 0 :accessor win-page)
   (feed-page :initform 0 :accessor win-feed-page)
   (view :initform :shows :accessor win-view
         :documentation "In a narrow window, which list is showing: :SHOWS or :EPISODES.")
   (mode :initform nil :accessor win-mode :documentation "NIL, or :ADD while a URL is typed.")
   (entry :initform "" :accessor win-entry)
   (confirm :initform nil :accessor win-confirm
            :documentation "The feed URL a first tap on Remove asked about.")
   (status :initform "" :accessor win-status)
   (log :initform '() :accessor win-log :documentation "Recent notes, newest first.")
   (busy :initform 0 :accessor win-busy :documentation "Background jobs in flight.")
   (lock :initform (sb-thread:make-mutex :name "spool-desk") :reader win-lock)
   (targets :initform '() :accessor win-targets)
   (rows :initform 1 :accessor win-rows :documentation "Episode rows per page, as last drawn.")
   (feed-rows :initform 1 :accessor win-feed-rows)
   (dirty :initform t :accessor win-dirty)
   (shown :initform nil :accessor win-shown :documentation "What the last repaint showed.")
   (checked :initform 0 :accessor win-checked :documentation "When the world was last looked at.")
   (pressed :initform nil :accessor win-pressed)
   (closed :initform nil :accessor win-closed))
  (:documentation "One podcast window: what it shows, and the hit areas of what it last drew."))

(defun %player () sg:*player*)

(defun %say (win fmt &rest args)
  (setf (win-status win) (apply #'format nil fmt args) (win-dirty win) t))

(defun %note (win fmt &rest args)
  "Say it in the status line AND keep it, so a failed subscribe can still be read after the next
   tap has rewritten the status line.  Called from background threads: must not signal."
  (let ((text (apply #'format nil fmt args)))
    (sb-thread:with-mutex ((win-lock win))
      (setf (win-status win) text
            (win-log win) (cons text (subseq (win-log win) 0 (min (length (win-log win))
                                                                   (1- *log-size*))))
            (win-dirty win) t))
    text))

(defun %busy (win delta)
  (sb-thread:with-mutex ((win-lock win))
    (incf (win-busy win) delta)
    (setf (win-dirty win) t)))

(defmacro %background ((win what) &body body)
  "Run BODY on its own thread, counted as busy, a failure reported rather than fatal (a desktop
   runs under --disable-debugger, where a condition escaping any thread takes the image)."
  (let ((w (gensym)) (n (gensym)))
    `(let ((,w ,win) (,n ,what))
       (%busy ,w 1)
       (%say ,w "~a..." ,n)
       (sb-thread:make-thread
        (lambda ()
          (unwind-protect
               (handler-case (progn ,@body)
                 (serious-condition (e) (%note ,w "~a failed: ~a" ,n e)))
            (%busy ,w -1)))
        :name (format nil "spool-desk ~a" ,n)))))

(defun %moving-p (win)
  (let ((p (%player)))
    (or (plusp (win-busy win))
        (and p (member (spool:player-state p) '(:playing :buffering :loading))))))

;;; ---- targets and buttons -------------------------------------------------------------------

(defun %target (win x y w h action &optional label)
  (push (make-instance 'target :x x :y y :w w :h h :action action :label label) (win-targets win)))

(defun %hit (win x y)
  "The target under (X,Y): the LAST drawn wins, as it is the one on top."
  (find-if (lambda (s) (and (<= (target-x s) x (+ (target-x s) (target-w s) -1))
                            (<= (target-y s) y (+ (target-y s) (target-h s) -1))))
           (win-targets win)))

(defun %button (win x y w h label action &key (color +btn-bg+))
  "A labelled rectangle and its target.  The label is set as large as fits, down to 11 px."
  (let ((fb (win-fb win)))
    (glass:fb-rect fb x y w h color)
    (glass:fb-frame fb x y w h +btn-edge+)
    (let* ((room (- w 10))
           (size (or (find-if (lambda (s) (<= (%width label s t) room)) '(15 14 13 12 11)) 11))
           (text (%fit label room :size size :bold t))
           (tw (%width text size t)))
      (%text fb (+ x (floor (- w tw) 2)) (+ y (floor (- h size) 2) -1) text :size size :bold t))
    (%target win x y w h (lambda (px py) (declare (ignore px py)) (funcall action)) label)))

(defun %button-row (win x y width specs &key (gap 8) (max 110))
  "SPECS ((label action &key color) ...) left to right in WIDTH, each as wide as fits up to MAX."
  (let* ((n (length specs))
         (w (min max (floor (- width (* gap (1- n))) (max 1 n)))))
    (loop for (label action . keys) in specs
          for bx from x by (+ w gap)
          do (apply #'%button win bx y w *button-height* label action keys))))

;;; ---- shapes --------------------------------------------------------------------------------

(defun %disc (fb cx cy r color)
  (loop for dy from (- r) to r
        for half = (isqrt (- (* r r) (* dy dy)))
        do (glass:fb-rect fb (- cx half) (+ cy dy) (1+ (* 2 half)) 1 color)))

(defun %ring (fb cx cy r color paper)
  "A circle's outline: a disc, and a smaller one in the row's own colour inside it."
  (%disc fb cx cy r color)
  (%disc fb cx cy (- r 2) paper))

(defun %state-icon (fb state cx cy paper)
  "State as a SHAPE, not a glyph: a missing glyph in a status column reads as a bug."
  (ecase state
    (:playing (loop for i from 0 to 12        ; a triangle, pointing right
                    do (glass:fb-rect fb (- cx 5) (+ cy -6 i) (1+ (- 6 (abs (- i 6)))) 1 +bar-fill+)))
    (:paused (glass:fb-rect fb (- cx 5) (- cy 6) 4 13 +bar-fill+)
             (glass:fb-rect fb (+ cx 1) (- cy 6) 4 13 +bar-fill+))
    (:downloading (%ring fb cx cy 7 +bar-fill+ paper)
                  (glass:fb-rect fb cx cy 1 6 +bar-fill+))
    (:cached (%disc fb cx cy 5 +dim+))
    (:played (glass:fb-rect fb (- cx 5) cy 11 2 +dim+))
    (:remote (%ring fb cx cy 6 +dim+ paper))))

;;; ---- the model -----------------------------------------------------------------------------

(defun %same-ep (a b)
  (and a b (string= (feed:episode-guid a) (feed:episode-guid b))))

(defun %episode-state (ep)
  (let ((p (%player)))
    (cond ((and p (%same-ep ep (spool:player-episode p)))
           (if (member (spool:player-state p) '(:playing :buffering :loading)) :playing :paused))
          ((spool:downloading-p ep) :downloading)
          ((spool:cached-p ep) :cached)
          ((spool:played-p ep) :played)
          (t :remote))))

(defun %pub-date (ep)
  (let ((u (feed:episode-published ep)))
    (if (null u)
        ""
        (multiple-value-bind (s m h day month year) (decode-universal-time u)
          (declare (ignore s m h))
          (format nil "~a ~d~@[ ~d~]"
                  (aref #("" "Jan" "Feb" "Mar" "Apr" "May" "Jun" "Jul" "Aug" "Sep" "Oct" "Nov" "Dec")
                        month)
                  day (and (/= year (nth-value 5 (get-decoded-time))) year))))))

(defun %pages (n per) (max 1 (ceiling n (max 1 per))))

(defun %page-of (items page per)
  "(values the items on PAGE (clamped) and the clamped page)."
  (let* ((n (length items))
         (page (max 0 (min page (1- (%pages n per)))))
         (start (* page per)))
    (values (subseq items (min n start) (min n (+ start per))) page)))

(defun %episodes (win) (if (win-feed win) (feed:feed-episodes (win-feed win)) '()))

(defun %visible-episodes (win)
  (values (%page-of (%episodes win) (win-page win) (win-rows win))))

(defun %progress (ep)
  "A download's progress as a whole percent, or NIL."
  (multiple-value-bind (got total) (spool:download-progress ep)
    (and got total (plusp total) (min 100 (floor (* 100 got) total)))))

(defun %signature (win moving)
  "Everything the window would draw that can change without a tap.  The episode states are read
   (a PROBE-FILE each) only while something is moving, which is when they change."
  (let ((p (%player)))
    (list (and p (spool:player-state p))
          (and p (or (spool:player-title p)
                     (let ((e (spool:player-episode p))) (and e (feed:episode-guid e)))))
          (and p (floor (spool:player-position p)))
          (and p (spool:player-duration p) (floor (spool:player-duration p)))
          (win-busy win)
          (length (spool:subscriptions))
          (and moving
               (loop for ep in (%visible-episodes win)
                     collect (let ((s (%episode-state ep)))
                               (if (eq s :downloading) (list s (%progress ep)) s)))))))

;;; ---- actions -------------------------------------------------------------------------------

(defun %select-feed (win f)
  (setf (win-feed win) f (win-page win) 0 (win-view win) :episodes (win-confirm win) nil)
  (%say win ""))

(defun %download (win ep &key then-play)
  "Fetch EP on its own thread; play it when it lands if THEN-PLAY.  Counted as busy, so the
   window keeps looking at the world until it is done."
  (%busy win 1)
  (spool:download-async
   ep
   :on-done (lambda (path) (declare (ignore path))
              (%busy win -1)
              (if then-play
                  (handler-case (progn (spool:play (sg:attach) ep)
                                       (%say win "Playing ~a" (feed:episode-title ep)))
                    (serious-condition (e) (%note win "Could not play ~a: ~a"
                                                  (feed:episode-title ep) e)))
                  (%note win "Downloaded ~a" (feed:episode-title ep))))
   :on-error (lambda (e) (%busy win -1) (%note win "Download failed: ~a" e)))
  (%say win "Downloading ~a (~,1f MB)..." (feed:episode-title ep)
        (/ (or (feed:episode-bytes ep) 0) 1048576.0)))

(defun %play-episode (win ep)
  "Tapping the episode that is playing pauses it — a listener tapping the highlighted row means
   \"stop talking\", never \"start this over\"."
  (let ((p (%player)))
    (cond ((and p (%same-ep ep (spool:player-episode p)))
           (spool:toggle p) (setf (win-dirty win) t))
          ((spool:downloading-p ep) (%say win "Still downloading ~a" (feed:episode-title ep)))
          ((spool:cached-p ep)
           (spool:play (sg:attach) ep)
           (%say win "Playing ~a" (feed:episode-title ep)))
          (t (%download win ep :then-play t)))))

(defun %get-episode (win ep)
  (cond ((spool:cached-p ep) (%say win "Already downloaded."))
        ((spool:downloading-p ep) (%say win "Already downloading."))
        (t (%download win ep))))

(defun %transport (win what)
  "A transport button.  With nothing loaded there is nothing to press, and saying so beats
   putting a silent source on the session's mixer."
  (let ((p (%player)))
    (if (or (null p) (and (null (spool:player-episode p)) (null (spool:player-title p))))
        (%say win "Nothing playing — tap an episode.")
        (case what
          (:toggle (spool:toggle p))
          (:back (spool:skip p -30))
          (:fwd (spool:skip p 30))
          (:next (let ((next (spool:next-episode p)))
                   (if next (%play-episode win next) (%say win "No next episode."))))))
    (setf (win-dirty win) t)))

(defun %seek (win frac)
  (let ((p (%player)))
    (cond ((null p) (%say win "Nothing playing — tap an episode."))
          ((spool:player-duration p) (spool:seek p (* frac (spool:player-duration p))))
          (t (%say win "No duration for this episode — cannot seek.")))
    (setf (win-dirty win) t)))

(defun %refresh-all (win)
  (%background (win "Refreshing all")
    (multiple-value-bind (ok bad) (spool:refresh-all)
      (let ((f (win-feed win)))
        (when f (setf (win-feed win) (find (feed:feed-url f) (spool:subscriptions)
                                           :key #'feed:feed-url :test #'string=))))
      (%note win "Refreshed ~d show~:p~[~:;, ~:*~d failed~]" ok (length bad)))))

(defun %refresh-feed (win)
  (let ((f (win-feed win)))
    (when f
      (%background (win "Refreshing")
        (let ((new (spool:refresh (feed:feed-url f))))
          (when (eq (win-feed win) f) (setf (win-feed win) new))
          (%note win "~a: ~d episodes" (feed:feed-title new) (length (feed:feed-episodes new))))))))

(defun %remove-feed (win)
  "Two taps: the first asks, the second removes.  One mistaken tap must not cost a subscription."
  (let ((f (win-feed win)))
    (when f
      (if (equal (win-confirm win) (feed:feed-url f))
          (progn (spool:unsubscribe (feed:feed-url f))
                 (setf (win-feed win) nil (win-confirm win) nil (win-view win) :shows)
                 (%note win "Removed ~a" (feed:feed-title f)))
          (progn (setf (win-confirm win) (feed:feed-url f))
                 (%say win "Tap Sure? to unsubscribe from ~a." (feed:feed-title f)))))))

(defun %cache-report (win)
  (%background (win "Measuring the cache")
    (multiple-value-bind (bytes files) (spool:cache-size)
      (%note win "~,1f GB in ~d file~:p at ~a" (/ bytes 1073741824.0) files (spool:library-root)))))

(defun %start-add (win)
  (setf (win-mode win) :add (win-entry win) "")
  (%say win "Type or paste the feed's URL."))

(defun %end-add (win) (setf (win-mode win) nil (win-dirty win) t))

(defun %paste (win)
  (let ((text (ignore-errors (glass:clipboard-text (glass:session-clipboard)))))
    (if (and text (plusp (length text)))
        (setf (win-entry win) (concatenate 'string (win-entry win)
                                           (remove-if (lambda (c) (member c '(#\Newline #\Return)))
                                                      text))
              (win-dirty win) t)
        (%say win "The clipboard is empty."))))

(defun %submit-add (win)
  (let ((url (string-trim '(#\Space #\Tab #\Newline #\Return) (win-entry win))))
    (%end-add win)
    (if (zerop (length url))
        (%say win "Nothing added.")
        ;; Both outcomes go through %NOTE, and a failure names the URL back: a typo is the
        ;; likeliest thing to go wrong here, and "HTTP 404" alone does not show it.
        (%background (win "Subscribing")
          (handler-case
              (let ((f (spool:subscribe url)))
                (setf (win-feed win) f (win-page win) 0 (win-view win) :episodes)
                (%note win "Added ~a (~d episodes)" (feed:feed-title f)
                       (length (feed:feed-episodes f))))
            (serious-condition (e) (%note win "Could not add ~a: ~a" url e)))))))

;;; ---- drawing -------------------------------------------------------------------------------

(defun %draw-transport (win)
  (let* ((fb (win-fb win)) (w (glass:fb-width fb))
         (p (%player))
         (ep (and p (spool:player-episode p)))
         (state (and p (spool:player-state p)))
         (pos (if p (spool:player-position p) 0))
         (dur (and p (spool:player-duration p)))
         (title (cond (ep (feed:episode-title ep))
                      ((and p (spool:player-title p)))
                      (t "Nothing playing"))))
    (glass:fb-rect fb 0 0 w *transport-height* +row-bg+)
    (%text fb 10 8 (%fit title (- w 20) :size 16 :bold t) :size 16 :bold t)
    ;; The bar, and one target over it (and a little around it) that seeks to where it was hit.
    (let* ((bx 10) (by 34) (bw (max 1 (- w 20))) (bh 14)
           (frac (if (and dur (plusp dur)) (min 1d0 (/ pos dur)) 0d0)))
      (glass:fb-rect fb bx by bw bh +bar-track+)
      (when (plusp frac) (glass:fb-rect fb bx by (round (* bw frac)) bh +bar-fill+))
      (%target win bx (- by 8) bw (+ bh 16)
               (lambda (x y) (declare (ignore y))
                 (%seek win (max 0d0 (min 1d0 (/ (- x bx) (float bw 1d0))))))))
    (%text fb 10 54 (format nil "~a / ~a~@[   ~(~a~)~]" (feed:format-duration pos)
                            (feed:format-duration dur)
                            (and state (not (member state '(:playing :idle))) state))
           :size 12 :color +dim+)
    (%button-row win 10 72 (- w 20)
                 `(("<< 30" ,(lambda () (%transport win :back)))
                   (,(if (eq state :playing) "Pause" "Play") ,(lambda () (%transport win :toggle))
                    :color ,(if (eq state :playing) +btn-hot+ +btn-bg+))
                   ("30 >>" ,(lambda () (%transport win :fwd)))
                   ("Next" ,(lambda () (%transport win :next)))
                   ("Refresh all" ,(lambda () (%refresh-all win))))
                 :max 120)
    (let ((msg (win-status win)))
      (when (plusp (length msg))
        (%text fb 10 (+ 72 *button-height* 5) (%fit msg (- w 20) :size 12) :size 12 :color +dim+)))
    (glass:fb-rect fb 0 (1- *transport-height*) w 1 +rule+)))

(defun %draw-feeds (win x y w h)
  "The shows list in the box X Y W H: header, a page of rows, and Add/Cache/paging buttons."
  (let* ((fb (win-fb win)) (subs (spool:subscriptions))
         (by (- (+ y h) *button-height* 6))
         (top (+ y 26))
         (per (max 1 (floor (- by 6 top) (1+ *row-height*)))))
    (setf (win-feed-rows win) per)
    (multiple-value-bind (rows page) (%page-of subs (win-feed-page win) per)
      (setf (win-feed-page win) page)
      (%text fb (+ x 8) (+ y 7) (format nil "SHOWS~[~:;  ~:*~d/~d~]" (if (> (length subs) per) (1+ page) 0)
                                        (%pages (length subs) per))
             :size 12 :bold t :color +dim+)
      (if (null subs)
          (%text fb (+ x 8) top "No shows yet: tap Add show." :size 13 :color +dim+)
          (loop for f in rows
                for ry from top by (1+ *row-height*)
                do (let ((sel (and (win-feed win) (string= (feed:feed-url f) (feed:feed-url (win-feed win)))))
                         (count (format nil "~d" (length (feed:feed-episodes f)))))
                     (glass:fb-rect fb x ry w *row-height* (if sel +row-sel+ +row-bg+))
                     (%text fb (+ x 8) (+ ry 9)
                            (%fit (or (feed:feed-title f) (feed:feed-url f)) (- w 24 (%width count 12 nil))
                                  :bold sel)
                            :bold sel)
                     (%text fb (- (+ x w) 8 (%width count 12 nil)) (+ ry 11) count :size 12 :color +dim+)
                     (let ((f f)) (%target win x ry w *row-height* (lambda (px py) (declare (ignore px py))
                                                                     (%select-feed win f)))))))
      ;; Add show and Cache share what the pager (two square buttons, when there is more than a
      ;; page of shows) leaves
      (let* ((paged (> (length subs) per))
             (pager (if paged (+ 6 (* 2 44) 6) 0)))
        (%button-row win (+ x 6) by (- w 12 pager)
                     `(("Add show" ,(lambda () (%start-add win)))
                       ("Cache" ,(lambda () (%cache-report win))))
                     :gap 6)
        (when paged
          (let ((px (- (+ x w) 6 (* 2 44) 6)))
            (%button win px by 44 *button-height* "<"
                     (lambda () (setf (win-feed-page win) (max 0 (1- page)) (win-dirty win) t)))
            (%button win (+ px 50) by 44 *button-height* ">"
                     (lambda () (setf (win-feed-page win) (1+ page) (win-dirty win) t)))))))))

(defun %draw-log (win x y w bottom)
  (loop for line in (win-log win)
        for ly from y by 22
        while (<= (+ ly 16) bottom)
        do (%text (win-fb win) x ly (%fit line w :size 12) :size 12 :color +dim+)))

(defun %draw-episodes (win x y w h &key back)
  "The selected show's episodes in the box X Y W H, paged, with Newer/Older/Refresh/Remove and
   the last note under them.  BACK: a narrow window's \"< Shows\" button in the header."
  (let* ((fb (win-fb win)) (f (win-feed win))
         (note-h (if (win-log win) 22 0))
         (by (- (+ y h) *button-height* 6 note-h))
         (hx (+ x 8)))
    (when back
      (%button win (+ x 6) (+ y 4) 96 32 "< Shows"
               (lambda () (setf (win-view win) :shows (win-confirm win) nil (win-dirty win) t)))
      (setf hx (+ x 112)))
    (if (null f)
        (progn (%text fb hx (+ y 12) "Pick a show on the left." :size 14 :color +dim+)
               (%draw-log win (+ x 10) (+ y 48) (- w 20) (+ y h)))
        (let* ((top (+ y (if back 42 30)))
               (per (max 1 (floor (- by 6 top) (1+ *row-height*))))
               (all (feed:feed-episodes f)))
          (setf (win-rows win) per)
          (multiple-value-bind (rows page) (%page-of all (win-page win) per)
            (setf (win-page win) page)
            (let ((info (format nil "~d episodes  ~d/~d" (length all) (1+ page) (%pages (length all) per))))
              (%text fb (- (+ x w) 8 (%width info 12 nil)) (+ y (if back 13 9)) info :size 12 :color +dim+)
              (%text fb hx (+ y (if back 11 7))
                     (%fit (feed:feed-title f) (- (+ x w) hx 20 (%width info 12 nil)) :size 15 :bold t)
                     :size 15 :bold t))
            (let ((show-date (>= w 520)))
              (loop for ep in rows
                    for i from 0
                    for ry from top by (1+ *row-height*)
                    do (let* ((state (%episode-state ep))
                              (ep ep)
                              (paper (cond ((member state '(:playing :paused)) +row-sel+)
                                           ((evenp i) +row-bg+)
                                           (t +row-alt+)))
                              (right (+ x w -6))
                              (get-w 56))
                         (glass:fb-rect fb x ry w *row-height* paper)
                         ;; the row plays; drawn first so the Get button over it wins the hit
                         (%target win x ry w *row-height*
                                  (lambda (px py) (declare (ignore px py)) (%play-episode win ep)))
                         (%state-icon fb state (+ x 18) (+ ry (floor *row-height* 2)) paper)
                         (let ((tail-x right))
                           (case state
                             ((:remote :played) (%button win (- right get-w) (+ ry 3) get-w (- *row-height* 6) "Get"
                                               (lambda () (%get-episode win ep)))
                                      (decf tail-x (+ get-w 8)))
                             (:downloading
                              (let ((s (format nil "~@[~d%~]" (%progress ep))))
                                (%text fb (- right (%width s 12 nil)) (+ ry 11) s :size 12 :color +bar-fill+)
                                (decf tail-x (+ get-w 8))))
                             (t (decf tail-x (+ get-w 8))))
                           (when show-date
                             (let ((d (%pub-date ep)))
                               (%text fb (- tail-x (%width d 12 nil)) (+ ry 11) d :size 12 :color +dim+)
                               (decf tail-x 92)))
                           (let ((d (feed:format-duration (feed:episode-seconds ep))))
                             (%text fb (- tail-x (%width d 12 nil)) (+ ry 11) d :size 12 :color +dim+)
                             (decf tail-x 62))
                           (%text fb (+ x 34) (+ ry 9) (%fit (feed:episode-title ep) (- tail-x x 34)))))))
            (let ((sure (equal (win-confirm win) (feed:feed-url f))))
              (%button-row win (+ x 6) by (- w 12)
                           `(("Newer" ,(lambda () (setf (win-page win) (max 0 (1- page)) (win-dirty win) t)))
                             ("Older" ,(lambda () (setf (win-page win) (1+ page) (win-dirty win) t)))
                             ("Refresh" ,(lambda () (%refresh-feed win)))
                             (,(if sure "Sure?" "Remove") ,(lambda () (%remove-feed win))
                              :color ,(if sure +btn-warn+ +btn-bg+)))
                           :max 130))
            (let ((note (first (win-log win))))
              (when note
                (%text fb (+ x 10) (- (+ y h) 20) (%fit note (- w 20) :size 12) :size 12 :color +dim+))))))))

(defun %draw-add (win x y w h)
  "Adding a show: an entry line, Paste / Add / Cancel, and the notes so far."
  (let ((fb (win-fb win)))
    (%text fb (+ x 12) (+ y 12) "Add a show" :size 16 :bold t)
    (%text fb (+ x 12) (+ y 36) "The feed's URL (RSS or Atom). Return adds it, Escape cancels." :size 12 :color +dim+)
    (let* ((ex (+ x 12)) (ey (+ y 58)) (ew (- w 24)) (eh 40)
           (room (- ew 20))
           (text (win-entry win))
           ;; the END of a long URL is what you are typing, so that is the part kept in view
           (shown (if (<= (%width text 16 nil) room)
                      text
                      (let ((k (length text)))
                        (loop while (and (> k 0) (<= (%width (subseq text (1- k)) 16 nil) (- room 20)))
                              do (decf k)
                              finally (return (concatenate 'string "..." (subseq text (min (length text) (1+ k))))))))))
      (glass:fb-rect fb ex ey ew eh #xffffff)
      (glass:fb-frame fb ex ey ew eh +bar-fill+ 2)
      (let ((end (%text fb (+ ex 10) (+ ey 11) shown :size 16)))
        (glass:fb-rect fb (+ (or end (+ ex 10)) 1) (+ ey 9) 2 22 +bar-fill+))
      (%button-row win ex (+ ey eh 12) ew
                   `(("Paste" ,(lambda () (%paste win)))
                     ("Add" ,(lambda () (%submit-add win)) :color ,+btn-hot+)
                     ("Cancel" ,(lambda () (%end-add win) (%say win "Nothing added."))))
                   :max 120)
      (%draw-log win (+ x 12) (+ ey eh 12 *button-height* 18) (- w 24) (+ y h)))))

(defun %redraw (win)
  (let* ((fb (win-fb win)) (w (glass:fb-width fb)) (h (glass:fb-height fb))
         (y0 *transport-height*) (bh (- h y0)))
    (setf (win-targets win) '())
    (glass:fb-fill fb +bg+)
    (%draw-transport win)
    (cond ((eq (win-mode win) :add) (%draw-add win 0 y0 w bh))
          ((>= w *narrow*)
           (let ((fw (min *feeds-width* (floor w 3))))
             (%draw-feeds win 0 y0 fw bh)
             (glass:fb-rect fb fw y0 1 bh +rule+)
             (%draw-episodes win (1+ fw) y0 (- w fw 1) bh)))
          ((and (eq (win-view win) :episodes) (win-feed win))
           (%draw-episodes win 0 y0 w bh :back t))
          (t (%draw-feeds win 0 y0 w bh)))))

;;; ---- input ---------------------------------------------------------------------------------

(defun %keysym-char (ks)
  (cond ((<= #x20 ks #x7e) (code-char ks))
        ((<= #xa0 ks #xff) (code-char ks))
        ((<= #x1000000 ks #x110ffff) (code-char (- ks #x1000000)))))

(defun %on-key (win down ks)
  (when down
    (if (eq (win-mode win) :add)
        (cond ((= ks #xff0d) (%submit-add win))                         ; Return
              ((= ks #xff1b) (%end-add win) (%say win "Nothing added.")) ; Escape
              ((= ks #xff08) (let ((e (win-entry win)))                 ; BackSpace
                               (when (plusp (length e)) (setf (win-entry win) (subseq e 0 (1- (length e)))))))
              (t (let ((ch (%keysym-char ks)))
                   (when ch (setf (win-entry win) (concatenate 'string (win-entry win) (string ch)))))))
        ;; A hardware keyboard gets the obvious keys; a phone never sends them.
        (cond ((= ks #x20) (%transport win :toggle))
              ((= ks #xff51) (%transport win :back))                    ; Left
              ((= ks #xff53) (%transport win :fwd))                     ; Right
              ((= ks #xff55) (setf (win-page win) (max 0 (1- (win-page win)))))  ; Prior
              ((= ks #xff56) (incf (win-page win)))                     ; Next
              ((= ks #xff1b) (setf (win-view win) :shows))))
    (setf (win-dirty win) t)))

(defun %on-pointer (win mask x y)
  "A press, not every move of a drag across the window: the left button going down."
  (let ((now (logbitp 0 mask)))
    (when (and now (not (win-pressed win)))
      (let ((target (%hit win x y)))
        (when target
          (let ((asked (win-confirm win)))
            (handler-case (funcall (target-action target) x y)
              (serious-condition (e) (%note win "~a" e)))
            ;; any tap but the second one on Remove (which clears it itself) takes the question back
            (when (and asked (equal asked (win-confirm win)))
              (setf (win-confirm win) nil)))
          (setf (win-dirty win) t))))
    (setf (win-pressed win) now)))

(defun %dirty-p (win)
  "Repaint iff something it shows changed: a tap or a background job (the DIRTY flag), or, looked
   at once a second, the player and the downloads (the signature)."
  (let ((now (get-internal-real-time)))
    (when (>= (- now (win-checked win)) internal-time-units-per-second)
      (setf (win-checked win) now)
      (let ((sig (%signature win (%moving-p win))))
        (unless (equal sig (win-shown win))
          (setf (win-shown win) sig (win-dirty win) t)))))
  (when (win-dirty win)
    (setf (win-dirty win) nil)
    (%redraw win)
    t))

;;; ---- the MAKE-FN ---------------------------------------------------------------------------

(defun podcasts-app (fb)
  "The podcast client in the glass window framebuffer FB, as glass.wm's WM-OPEN takes an app:
   (values ON-KEY ON-POINTER DIRTY-P COPY-P CLOSE-FN RESIZE-FN TEXT-P SELECTION-FN).  SELECTION-FN
   (what is highlighted, for Speak selection) is NIL: nothing here is selectable text.  Returns the
   window as a NINTH value, for a test or a REPL to look at."
  (let ((win (make-instance 'podcasts-window :fb fb)))
    (values (lambda (down ks) (%on-key win down ks))
            (lambda (mask x y) (%on-pointer win mask x y))
            (lambda () (%dirty-p win))
            nil
            (lambda () (setf (win-closed win) t))     ; the show plays on: it is the mixer's
            (lambda (w h) (glass:fb-resize fb w h +bg+) (setf (win-dirty win) t))
            nil
            nil                                        ; SELECTION-FN
            win)))

(defparameter *podcasts-app* `("Podcasts" podcasts-app :width ,*default-width* :height ,*default-height*)
  "The client as SERVE-DESK / SERVE-WM take applications: (LABEL MAKE-FN &key WIDTH HEIGHT).")
