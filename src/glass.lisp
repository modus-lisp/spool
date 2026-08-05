;;;; glass.lisp — the show, on the box's speakers.
;;;;
;;;; This file is four functions long, and that is the point: the player already
;;;; produces exactly what a glass mixer consumes, so "play a podcast through the
;;;; desktop" is a subscription, not an integration.  Everything interesting —
;;;; that one mix is shared, that a WebRTC peer and a VNC viewer each get their own
;;;; cursor and rate, that a stalled listener drops frames instead of stalling the
;;;; others — already happened in glass/audio and is not restated here.
;;;;
;;;; ATTACH takes a mixer rather than making one when it can, because the box HAS a
;;;; mixer: the session's.  Making a second one would give the podcast its own
;;;; private audience of nobody, which is the exact failure glass's mixer header
;;;; warns about.

(in-package #:spool.glass)

(defvar *mixer* nil "The mixer spool is playing into.")
(defvar *player* nil "The one player.  A box plays one show at a time.")
(defvar *source-id* nil "Our registration on *MIXER*, so DETACH can undo exactly it.")
(defvar *lock* (sb-thread:make-mutex :name "spool-glass"))

(defun %box-mixer ()
  "The session's mixer — glass's own, so the podcast goes wherever the session's
sound goes (a VNC viewer, every WebRTC peer) rather than into a private mix with
an audience of nobody.  GLASS:SESSION-MIXER creates and starts it on first call
and is idempotent under a race, so this is also correct in an image where the
desktop has not started audio yet."
  (or *mixer* (glass:session-mixer)))

(defun attach (&key mixer (gain 1.0d0) on-end)
  "Put spool's audio on MIXER (or the box's) and return the player.  Idempotent:
calling it twice does not register a second source, because the source thunk stays
valid across episodes and re-registering would double the volume of the same show."
  (sb-thread:with-mutex (*lock*)
    (unless *player*
      (setf *player* (spool:make-player :on-end (or on-end #'%advance))))
    (unless *mixer*
      (setf *mixer* (or mixer (%box-mixer))))
    (unless *source-id*
      (setf *source-id*
            (glass:mixer-add-source *mixer* (spool:player-source *player*)
                                    :name "podcast" :gain gain)))
    *player*))

(defun detach ()
  "Take spool off the mixer.  The player keeps its position, so ATTACH resumes."
  (sb-thread:with-mutex (*lock*)
    (when (and *mixer* *source-id*)
      (glass:mixer-remove-source *mixer* *source-id*))
    (setf *source-id* nil))
  t)

(defun %advance (p)
  "Default end-of-episode policy: roll on to the next one in the same show, if it
is already downloaded.  If it is not, stop — starting a multi-hundred-megabyte
download because a listener walked away is not a favour."
  (let ((next (spool:next-episode p)))
    (when (and next (spool:cached-p next))
      (spool:play p next))))

(defun ensure-playing (episode &key (download t))
  "Play EPISODE on the box, fetching it first if it is not cached.  Returns
:playing, or :downloading if it went to fetch — the UI's two honest answers."
  (let ((p (attach)))
    (cond
      ((spool:cached-p episode) (spool:play p episode) :playing)
      (download
       (spool:download-async episode :on-done (lambda (path) (declare (ignore path))
                                                (spool:play p episode)))
       :downloading)
      (t :not-cached))))

(defun report ()
  (format nil "~a~@[ | ~a~]"
          (if *player* (spool:player-report *player*) "no player")
          (and *mixer* (glass:mixer-report *mixer*))))
