;;;; inspect/app-drive.lisp — drive the CLIM client with a synthetic pointer.
;;;;
;;;;   sbcl --control-stack-size 256 --dynamic-space-size 4096 --load inspect/app-drive.lisp
;;;;
;;;; The client's whole job is to be CLICKABLE from a phone over a scaled remote
;;;; framebuffer, so a test that calls its commands directly would skip the only
;;;; part that is actually in question.  This boots a private glass desktop with no
;;;; RFB server (nothing to connect to, nothing for a listener to see), spawns the
;;;; frame in it, and injects pointer presses at real screen coordinates through the
;;;; same GLASS-ON-POINTER the VNC path uses — WM hit-testing, presentation lookup
;;;; and command dispatch all included.  Then it looks at the state the click was
;;;; supposed to change, and writes the framebuffer to a PNG so a human can see what
;;;; the phone would have seen.
;;;;
;;;; It uses the REAL library, and only ever clicks episodes that are already in the
;;;; cache: a UI probe that starts a 190 MB download is not a probe.

(require :asdf)
(load "~/quicklisp/setup.lisp")
(handler-bind ((warning #'muffle-warning))
  (let ((*standard-output* (make-broadcast-stream)))
    (ql:quickload '(:mcclim :mcclim-render :pigment))
    (asdf:load-asd "/home/claude/glass/backend/mcclim-glass.asd")
    (asdf:load-system :mcclim-glass)
    (asdf:load-system :spool/app)))

(defpackage #:spool-drive (:use #:cl) (:local-nicknames (#:feed #:spool.feed) (#:app #:spool.app)))
(in-package #:spool-drive)

(defparameter *shots* "/tmp/spool-app-")
(defvar *port* nil)
(defvar *frame* nil)
(defvar *pass* 0)
(defvar *fail* 0)

(defun ok (name got &optional (expected t) (test #'eql))
  (if (funcall test expected got)
      (progn (incf *pass*) (format t "~&  ok   ~a~@[ = ~a~]~%" name (and (not (eq got t)) got)))
      (progn (incf *fail*) (format t "~&  FAIL ~a: got ~s, wanted ~s~%" name got expected))))

;;; ---- a desktop with no audience --------------------------------------------

(defun boot (&key (width 1000) (height 700))
  (let ((p (clim-glass::find-glass-port :port 5916)))
    (setf (clim-glass::glass-port-wm-p p) t
          (clim-glass::glass-port-screen-w p) width
          (clim-glass::glass-port-screen-h p) height
          (clim-glass::glass-port-fb p) (glass:make-framebuffer width height)
          (clim-glass::glass-port-menu-items p) (clim-glass::wm-default-menu))
    ;; No START-GLASS-SERVER: the framebuffer is the output, and opening an RFB
    ;; listener would collide with a real desktop on a shared box.
    (climi::restart-port p)
    (setf *port* p)))

(defun spawn ()
  (clim-glass::wm-spawn-spec *port* (list 'spool.app::podcasts :width 900 :height 620))
  (loop repeat 100
        until (find-if (lambda (m) (search "Podcast" (clim-glass::glass-mirror-title m)))
                       (clim-glass::glass-port-mirrors *port*))
        do (sleep 0.1))
  (sleep 1.5)                           ; let the first redisplay finish
  (let ((m (find-if (lambda (m) (search "Podcast" (clim-glass::glass-mirror-title m)))
                    (clim-glass::glass-port-mirrors *port*))))
    (clim-glass::wm-move m 20 40)
    (setf *frame* (clim:pane-frame (clim-glass::glass-mirror-sheet m)))
    (clim-glass::composite-all *port*)
    m))

(defun mirror ()
  (clim:sheet-direct-mirror (clim:frame-top-level-sheet *frame*)))

(defun shot (name)
  (let* ((fb (clim-glass::glass-port-fb *port*))
         (px (glass:fb-pixels fb)) (w (glass:fb-width fb)) (h (glass:fb-height fb))
         (raw (format nil "/tmp/spool-shot.raw")))
    (with-open-file (s raw :direction :output :element-type '(unsigned-byte 8)
                           :if-exists :supersede)
      (dotimes (i (* w h))
        (let ((v (aref px i)))
          (write-byte (ldb (byte 8 16) v) s)
          (write-byte (ldb (byte 8 8) v) s)
          (write-byte (ldb (byte 8 0) v) s))))
    (let ((png (format nil "~a~a.png" *shots* name)))
      (uiop:run-program
       (list "python3" "-c"
             (format nil "from PIL import Image;Image.frombytes('RGB',(~d,~d),open('~a','rb').read()).save('~a')"
                     w h raw png))
       :ignore-error-status t)
      png)))

;;; ---- clicking --------------------------------------------------------------

(defun click (pane px py &key (settle 0.6))
  "Press and release at (PX,PY) in PANE's own coordinates, through the same entry
point the RFB server calls."
  (multiple-value-bind (nx ny)
      (clim:transform-position (clim:sheet-native-transformation pane) px py)
    (let* ((m (mirror))
           (sx (round (+ (clim-glass::glass-mirror-x m) nx)))
           (sy (round (+ (clim-glass::glass-mirror-y m) ny))))
      (clim-glass::glass-on-pointer *port* 1 sx sy)
      (sleep 0.15)
      (clim-glass::glass-on-pointer *port* 0 sx sy)
      (sleep settle)
      (clim-glass::composite-all *port*)
      (list sx sy))))

(defun pane (name) (clim:find-pane-named *frame* name))

;;; ---- typing ----------------------------------------------------------------
;;;
;;; Keys reach the focused window, which is where the RFB server sends them too, so
;;; a prompt that cannot be answered or escaped shows up here as a hang rather than
;;; as a passing test.

(defun key (keysym &key (settle 0.05))
  (clim-glass::glass-on-key *port* t keysym)
  (clim-glass::glass-on-key *port* nil keysym)
  (sleep settle))

(defun typing (string &key (enter t) (settle 0.6))
  (loop for c across string do (key (char-code c)))
  (when enter (key #xff0d))
  (sleep settle)
  (clim-glass::composite-all *port*))

;;; ---- the drive -------------------------------------------------------------

(defun row-y (index) (+ 24 (* index (1+ spool.app::*row-height*)) (floor spool.app::*row-height* 2)))

(defun rows () (spool.app::app-rows *frame*))

(defun cached-index (feed)
  "Position of the first already-downloaded episode in FEED, or NIL."
  (position-if #'spool:cached-p (feed:feed-episodes feed)))

(defun run ()
  (format t "~&== spool CLIM client, driven by pointer ==~%")
  (boot)
  (spawn)
  (format t "~&frame ~a at ~a,~a~%  ~a~%" (type-of *frame*)
          (clim-glass::glass-mirror-x (mirror)) (clim-glass::glass-mirror-y (mirror))
          (shot "1-open"))
  (let ((feeds (spool:subscriptions)))
    (when (null feeds)
      (format t "~&no subscriptions in ~a — nothing to drive.~%" spool:*library-root*)
      (return-from run))

    ;; 1. a show
    (click (pane 'spool.app::feeds) 120 (row-y 0))
    (ok "clicking a show selects it"
        (and (app::app-feed *frame*) (feed:feed-title (app::app-feed *frame*)))
        (feed:feed-title (first feeds)) #'equal)
    (shot "2-show")

    ;; 2. an episode that is already downloaded (paged to, if need be)
    (let* ((f (app::app-feed *frame*))
           (idx (cached-index f)))
      (cond
        ((null idx) (format t "~&  -- no cached episode in ~a; skipping playback~%"
                            (feed:feed-title f)))
        (t
         (setf (app::app-page *frame*) (floor idx (rows)))
         (clim:execute-frame-command *frame* '(spool.app::com-tick))
         (sleep 0.5)
         (click (pane 'spool.app::episodes) 200 (row-y (mod idx (rows))))
         (let ((p (app::app-player *frame*)))
           (ok "clicking an episode starts it" (spool:player-state p) :playing)
           (ok "the mixer got the show's source"
               (search "podcast" (glass:mixer-report spool.glass:*mixer*)) t
               (lambda (want got) (declare (ignore want)) (numberp got)))
           (shot "3-playing")

           ;; 3. the transport
           (let ((before (spool:player-position p)))
             (click (pane 'spool.app::transport) 240 85)   ; 30 >>
             (ok "30 >> moves forward ~30 s"
                 (round (- (spool:player-position p) before)) 30
                 (lambda (want got) (< (abs (- want got)) 3))))
           (click (pane 'spool.app::transport) 140 85)     ; Play/Pause
           (ok "Pause pauses" (spool:player-state p) :paused)
           (ok "a paused source hands out silence, not a stall"
               (loop repeat 5 always (null (funcall (spool:player-source p)))) t)
           (shot "4-paused")

           ;; 4. the seek bar: tap at the 3/4 mark
           (let* ((tr (pane 'spool.app::transport))
                  (w (clim:bounding-rectangle-width (clim:sheet-region tr)))
                  (dur (spool:player-duration p)))
             (click tr (+ 10 (* 0.75 (- w 20))) 37)
             (ok "tapping the bar seeks there"
                 (/ (spool:player-position p) dur) 0.75
                 (lambda (want got) (< (abs (- want got)) 0.03))))
           (shot "5-seeked")

           ;; 5. paging
           (let ((page (app::app-page *frame*)))
             (click (pane 'spool.app::episodes)
                    150 (+ 24 (* (1+ spool.app::*row-height*) (rows)) 25))
             (ok "Older turns the page" (app::app-page *frame*) (1+ page)))
           (shot "6-paged")

           (spool:pause p)
           (spool.glass:detach)))))

    ;; 6. Add show.  The button prompts, which BLOCKS the frame, so the thing worth
    ;; testing is not that it asks — it is that every way out gets the frame back.
    ;; This never subscribes to anything real: one answer is empty, the other is a
    ;; URL nothing is listening on.
    (let ((n (length feeds)))
      (flet ((tap-add ()
               (click (pane 'spool.app::feeds) 60
                      (+ 24 (* n (1+ spool.app::*row-height*)) 23))))
        (tap-add)
        (ok "Add show asks for a URL"
            (search "feed URL" (app::app-status *frame*)) t
            (lambda (want got) (declare (ignore want)) (numberp got)))
        (shot "7-asking")
        (typing "")
        (ok "an empty answer cancels" (app::app-status *frame*) "Nothing added." #'equal)
        (let ((page (app::app-page *frame*)))
          (click (pane 'spool.app::episodes)
                 60 (+ 24 (* (1+ spool.app::*row-height*) (rows)) 25))
          (ok "and the frame still answers the pointer afterwards"
              (app::app-page *frame*) (max 0 (1- page))))

        (tap-add)
        (let ((url "http://127.0.0.1:1/not-a-feed"))
          (typing url)
          (ok "a URL nothing answers reports, rather than hanging or dying"
              (loop repeat 60
                    thereis (search "Could not add" (app::app-status *frame*))
                    do (sleep 0.25))
              t (lambda (want got) (declare (ignore want)) (numberp got)))
          ;; Character for character, because the failure this gate exists to catch is a
          ;; DROPPED KEYSTROKE: a URL one character short 404s and reports, so a test that
          ;; only asks "did it report?" passes just as happily on a mangled URL.  It also
          ;; pins the other half — that the report names the URL at all, without which
          ;; "it didn't work" has nothing to look at.
          (ok "and the report quotes the URL exactly as typed"
              (search url (app::app-status *frame*)) t
              (lambda (want got) (declare (ignore want)) (numberp got)))
          ;; The original bug wasn't only that it failed — it was that the reason was
          ;; gone by the time anyone looked, because the next tap rewrote the one line
          ;; it was written on.  So: tap something, then ask again.
          (click (pane 'spool.app::transport) 140 85)
          (ok "and the reason outlives the next tap"
              (find-if (lambda (line) (search url line)) (app::app-log *frame*)) t
              (lambda (want got) (declare (ignore want)) (stringp got))))
        (ok "and the library is untouched" (length (spool:subscriptions)) n)
        (shot "8-add-failed"))))
  (format t "~&== ~d passed, ~d failed ==~%" *pass* *fail*)
  (finish-output))

(run)
(sb-ext:exit :code (if (plusp *fail*) 1 0))
