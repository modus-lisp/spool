;;;; inspect/desk-app-test.lisp — drive the glass-window client (spool/desk) headless.
;;;;
;;;;   sbcl --non-interactive --load inspect/desk-app-test.lisp [out-dir]
;;;;
;;;; No network, no audio, no window manager: the MAKE-FN is called on a bare glass framebuffer,
;;;; exactly as WM-OPEN calls it, and pressed at real window coordinates through its ON-POINTER —
;;;; buttons are found by their label in the window's own hit list, the way a person finds them
;;;; by reading the screen, and then pressed at their centre.  The library is a temporary root
;;;; filled from the fixture feed and synthetic ones; one episode is given a fake cached file and
;;;; one a player that is "playing" (no reed, no mixer), so every row state is drawn.  PNGs of the
;;;; window go to OUT-DIR (default: the session scratchpad), through ImageMagick's convert.

(require :asdf)
(load "~/quicklisp/setup.lisp")
(dolist (d '("spool/" "glass/" "reed/" "weft/" "scribe/" "pigment/" "cram/"))
  (let ((p (merge-pathnames d #p"/home/claude/modus-lisp/")))
    (when (probe-file p) (pushnew p asdf:*central-registry* :test #'equal))))
(handler-bind ((warning #'muffle-warning))
  (let ((*standard-output* (make-broadcast-stream)))
    (asdf:load-system :spool/desk)))

(defpackage #:spool-desk-test
  (:use #:cl)
  (:local-nicknames (#:feed #:spool.feed) (#:d #:spool.desk)))
(in-package #:spool-desk-test)

(defparameter *out*
  (let ((arg (second sb-ext:*posix-argv*)))
    (pathname (or (and arg (not (search "--" arg)) arg)
                  "/tmp/claude-1001/-home-claude/b54f831c-c163-453e-842f-90f9ae1cad44/scratchpad/spool/"))))
(ensure-directories-exist *out*)

(defvar *pass* 0)
(defvar *fail* 0)

(defun ok (name got &optional (expected t) (test #'equal))
  (if (funcall test expected got)
      (progn (incf *pass*) (format t "~&  ok   ~a~@[ = ~s~]~%" name (and (not (eq got t)) got)))
      (progn (incf *fail*) (format t "~&  FAIL ~a: got ~s, wanted ~s~%" name got expected))))

;;; ---- a library with no network -------------------------------------------------------------

(defparameter *root* (merge-pathnames "library/" *out*))
(uiop:delete-directory-tree *root* :validate t :if-does-not-exist :ignore)
(setf spool:*library-root* (ensure-directories-exist *root*)
      spool::*library* nil)

(defun fixture (name)
  (uiop:read-file-string (merge-pathnames (format nil "fixtures/~a" name)
                                          (directory-namestring *load-truename*))))

(defun synthetic (title n &key (url (format nil "https://example.invalid/~(~a~).xml" (remove #\Space title))))
  (feed:make-feed
   :title title :url url
   :episodes (loop for i from n downto 1
                   collect (feed:make-episode
                            :guid (format nil "~a-~d" url i)
                            :title (format nil "~a ~d: a reasonably long episode title that will need cutting to fit" title i)
                            :url (format nil "https://example.invalid/audio/~d.mp3" i)
                            :bytes 100000 :seconds (+ 600 (* 37 i))
                            :published (- (get-universal-time) (* i 86400 7))
                            :feed-url url))))

(defun install (f)
  (let ((lib (spool:library)))
    (setf (gethash (feed:feed-url f) (spool::library-feeds lib)) f)
    (setf (spool::library-order lib) (append (spool::library-order lib) (list (feed:feed-url f))))
    f))

(defparameter *fixture* (install (feed:parse-feed (fixture "rss.xml") :url "https://example.invalid/rss.xml")))
(defparameter *big* (install (synthetic "Long Running Show" 40)))
(loop for i from 1 to 14 do (install (synthetic (format nil "Show ~2,'0d" i) (+ 3 i))))

;;; ---- driving it ----------------------------------------------------------------------------

(defstruct app on-key on-pointer dirty-p resize win)

(defun open-app (w h)
  (let ((fb (glass:make-framebuffer w h glass:+white+)))
    (multiple-value-bind (on-key on-pointer dirty-p copy-p close-fn resize-fn text-p selection-fn win) (d:podcasts-app fb)
      (declare (ignore copy-p close-fn selection-fn))
      (ok (format nil "~dx~d: make-fn contract (text-p nil, resize-fn)" w h)
          (list (functionp on-key) (functionp on-pointer) (functionp dirty-p) (functionp resize-fn) text-p)
          '(t t t t nil))
      (values (make-app :on-key on-key :on-pointer on-pointer :dirty-p dirty-p :resize resize-fn :win win) fb))))

(defun paint (a) (funcall (app-dirty-p a)))

(defun button (a label)
  (find label (d:win-targets (app-win a)) :key #'d:target-label :test #'equal))

(defun tap-at (a x y)
  (funcall (app-on-pointer a) 1 x y)
  (funcall (app-on-pointer a) 0 x y)
  (paint a))

(defun tap (a label)
  "Press the button labelled LABEL at its centre; NIL if it is not on screen."
  (let ((b (button a label)))
    (when b
      (tap-at a (+ (d:target-x b) (floor (d:target-w b) 2)) (+ (d:target-y b) (floor (d:target-h b) 2)))
      t)))

(defun keys (a string)
  (loop for ch across string do (funcall (app-on-key a) t (char-code ch)) (funcall (app-on-key a) nil (char-code ch))))

(defun png (fb name)
  "FB as OUT-DIR/NAME.png (through a PPM and convert)."
  (let ((ppm (merge-pathnames (format nil "~a.ppm" name) *out*))
        (out (merge-pathnames (format nil "~a.png" name) *out*))
        (w (glass:fb-width fb)) (h (glass:fb-height fb)))
    (with-open-file (s ppm :direction :output :element-type '(unsigned-byte 8) :if-exists :supersede)
      (loop for c across (map 'vector #'char-code (format nil "P6 ~d ~d 255~%" w h)) do (write-byte c s))
      (let ((row (make-array (* 3 w) :element-type '(unsigned-byte 8))))
        (dotimes (y h)
          (dotimes (x w)
            (let ((p (glass:fb-get fb x y)))
              (setf (aref row (* 3 x)) (ldb (byte 8 16) p)
                    (aref row (+ 1 (* 3 x))) (ldb (byte 8 8) p)
                    (aref row (+ 2 (* 3 x))) (ldb (byte 8 0) p))))
          (write-sequence row s))))
    (uiop:run-program (list "convert" (namestring ppm) (namestring out)))
    (delete-file ppm)
    (format t "~&  png  ~a~%" (namestring out))
    out))

(defun distinct-colours (fb)
  (let ((seen (make-hash-table)))
    (dotimes (y (glass:fb-height fb)) (dotimes (x (glass:fb-width fb)) (setf (gethash (glass:fb-get fb x y) seen) t)))
    (hash-table-count seen)))

;;; ---- the wide window -----------------------------------------------------------------------

(format t "~&wide window (980x660)~%")
(multiple-value-bind (a fb) (open-app 980 660)
  (let ((win (app-win a)))
    (ok "first DIRTY-P paints" (paint a))
    (ok "it painted something (more than a few colours)" (> (distinct-colours fb) 20))
    (ok "second DIRTY-P with nothing changed does not" (paint a) nil)
    (png fb "desk-wide-empty")
    (ok "feeds are paged: 16 shows, a > button" (and (button a ">") t))
    (let ((per (spool.desk::win-feed-rows win)))
      (ok "shows rows per page" per 12)
      (tap a ">")
      (ok "> goes to page 2 of the shows" (d:win-feed-page win) 1)
      (tap a ">")
      (ok "> again stays on the last page" (d:win-feed-page win) 1)
      (tap a "<")
      (ok "< back to page 1" (d:win-feed-page win) 0))
    ;; the second show's row, at its place on the screen: below the transport and SHOWS header
    (tap-at a 100 (+ spool.desk::*transport-height* 26 (1+ spool.desk::*row-height*) 18))
    (ok "tapping the second show's row selects it" (and (d:win-feed win) (feed:feed-title (d:win-feed win)))
        "Long Running Show")
    (let ((rows (d:win-rows win)))
      (ok "episode rows measured from the window" rows 12)
      (ok "page 1" (d:win-page win) 0)
      (tap a "Older") (ok "Older -> page 2" (d:win-page win) 1)
      (tap a "Older") (tap a "Older") (tap a "Older") (tap a "Older")
      (ok "Older stops at the last page" (d:win-page win) (1- (ceiling 40 rows)))
      (tap a "Newer") (ok "Newer goes back one" (d:win-page win) (- (ceiling 40 rows) 2))
      (tap a "Newer") (tap a "Newer") (tap a "Newer") (tap a "Newer")
      (ok "Newer stops at the first page" (d:win-page win) 0))
    (ok "every episode on the page has a Get button (none cached, none played)"
        (count "Get" (d:win-targets win) :key #'d:target-label :test #'equal) (d:win-rows win))
    ;; transport with nothing loaded: says so, and does not put a source on a mixer
    (tap a "Play")
    (ok "Play with nothing loaded explains" (d:win-status win) "Nothing playing — tap an episode.")
    (ok "... and did not make a player" spool.glass:*player* nil)
    (tap-at a 400 40)
    (ok "a tap on the seek bar with nothing loaded explains" (d:win-status win) "Nothing playing — tap an episode.")
    ;; Remove takes two taps, and anything in between takes the question back
    (tap a "Remove")
    (ok "first Remove asks" (and (d:win-confirm win) (button a "Sure?") t))
    (tap a "Older")
    (ok "another tap takes the question back" (list (d:win-confirm win) (button a "Sure?")) '(nil nil))
    (tap a "Remove") (tap a "Sure?")
    (ok "Remove, Sure? unsubscribes" (find "Long Running Show" (spool:subscriptions)
                                           :key #'feed:feed-title :test #'string=) nil)
    (ok "... and the list is empty again" (d:win-feed win) nil)
    (install *big*)
    ;; adding a show: a mode with an entry line that takes keys
    (tap a "Add show")
    (ok "Add show enters the entry mode" (d:win-mode win) :add)
    (keys a "https://example.invalid/feed.xmlx")
    (funcall (app-on-key a) t #xff08)          ; BackSpace
    (ok "typed, with a backspace" (d:win-entry win) "https://example.invalid/feed.xml")
    (paint a)
    (png fb "desk-wide-add")
    (tap a "Cancel")
    (ok "Cancel leaves the mode" (list (d:win-mode win) (d:win-status win)) '(nil "Nothing added."))
    (tap a "Add show") (tap a "Add")
    (ok "Add with an empty line adds nothing" (list (d:win-mode win) (d:win-status win)) '(nil "Nothing added."))
    ;; every row state, for the picture: one cached, one played, one "playing" (no reed, no mixer)
    (tap-at a 100 (+ spool.desk::*transport-height* 26 18))  ; the fixture show, first in the list
    (let* ((eps (feed:feed-episodes (first (spool:subscriptions))))
           (cached (second eps)))
      (with-open-file (s (ensure-directories-exist (spool:cache-path cached)) :direction :output
                         :element-type '(unsigned-byte 8) :if-exists :supersede)
        (write-sequence (make-array (max 70000 (or (feed:episode-bytes cached) 0)) :element-type '(unsigned-byte 8) :initial-element 0) s))
      (spool:mark-played (third eps))
      (let ((p (spool:make-player)))
        (setf (spool::player-episode p) (first eps) (spool::player-state p) :playing
              (spool::player-duration p) 3000d0 (spool::player-base p) 1234d0
              spool.glass:*player* p)
        (setf (spool.desk::win-checked win) 0)    ; let the once-a-second look happen now
        (ok "a playing player is noticed without a tap" (paint a))
        (ok "Pause shows while playing" (and (button a "Pause") t))
        (png fb "desk-wide-playing")
        (ok "Get on all but the playing and the cached episode (a played one can be fetched again)"
            (count "Get" (d:win-targets win) :key #'d:target-label :test #'equal)
            (- (min (d:win-rows win) (length eps)) 2))
        (tap-at a 500 (+ spool.desk::*transport-height* 30 18))
        (ok "tapping the playing row pauses it, not restarts it" (spool:player-state p) :paused)
        (setf spool.glass:*player* nil)))))

;;; ---- the narrow window ---------------------------------------------------------------------

(format t "~&narrow window (400x700)~%")
(multiple-value-bind (a fb) (open-app 400 700)
  (let ((win (app-win a)))
    (ok "paints" (paint a))
    (ok "one list at a time: no Newer button with the shows up" (button a "Newer") nil)
    (png fb "desk-narrow-shows")
    (tap-at a 200 (+ spool.desk::*transport-height* 26 18))
    (ok "tapping a show opens its episodes" (list (d:win-view win) (and (button a "< Shows") t)) '(:episodes t))
    (png fb "desk-narrow-episodes")
    (tap a "Older")
    (ok "Older on a one-page show stays on page 1" (d:win-page win) 0)
    (tap a "< Shows")
    (ok "< Shows goes back" (list (d:win-view win) (and (button a "Add show") t)) '(:shows t))
    ;; and a wide window resized narrow by the window manager
    (funcall (app-resize a) 980 660)
    (ok "RESIZE-FN repaints at the new size" (list (paint a) (glass:fb-width fb) (glass:fb-height fb))
        '(t 980 660))
    (ok "two columns again" (and (button a "Add show") (button a "Older") t))))

;;; ---- in a real window manager, when glass/wm loads -----------------------------------------
;;; Not spool's dependency (spool/desk needs only glass/text): loaded here, and skipped with a
;;; note if it will not load, so the test above still stands on its own.

(format t "~&glass.wm (WM-OPEN, WM-TICK)~%")
(let ((loaded (handler-case
                  (handler-bind ((warning #'muffle-warning))
                    (let ((*standard-output* (make-broadcast-stream)))
                      (asdf:load-system :glass/wm) t))
                (serious-condition (e) (format t "~&  skip glass/wm did not load: ~a~%" e) nil))))
  (when loaded
    (let* ((screen (glass:make-framebuffer 1100 800 glass:+black+))
           (wm (funcall (find-symbol "MAKE-WM" "GLASS.WM") screen)))
      (destructuring-bind (label make-fn &key width height) d:*podcasts-app*
        (funcall (find-symbol "WM-REGISTER-APP" "GLASS.WM") wm label make-fn :width width :height height)
        (let ((s (funcall (find-symbol "WM-OPEN" "GLASS.WM") wm label)))
          (ok "WM-OPEN opens the Podcasts window" (and s t))
          (ok "WM-TICK paints it" (funcall (find-symbol "WM-TICK" "GLASS.WM") wm))
          (png screen "desk-in-wm"))))))

(format t "~&~d passed, ~d failed~%" *pass* *fail*)
(uiop:quit (if (zerop *fail*) 0 1))
