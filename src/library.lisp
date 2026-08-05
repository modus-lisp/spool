;;;; library.lisp — what you are subscribed to, where you got to, and the audio on disk.
;;;;
;;;; Three separable things that all want the same root directory, so they live
;;;; together: the subscription list, the playback positions, and the episode cache.
;;;;
;;;; The library is persisted as ONE s-expression file, written to a temporary name
;;;; and renamed over the old one.  Rename is atomic on the filesystems this runs on,
;;;; so a crash mid-save leaves the previous library intact rather than a half-written
;;;; one — and "your subscriptions are now an empty file" is exactly the failure a
;;;; podcast client must not have.  The parsed feeds are persisted along with the
;;;; subscriptions, not just their URLs, so the app opens instantly and offline; a
;;;; refresh is then something the user asks for, not something they wait through.
;;;;
;;;; The cache does NOT live under the home directory.  Episodes run 25-190 MB (a
;;;; three-hour interview is 186 MB), and this box's root filesystem has single-digit
;;;; gigabytes free — thirty episodes would fill it and take the desktop, the web
;;;; service and the git server down with it.  So the default root is on the big
;;;; volume, and SPOOL_ROOT overrides it.

(in-package #:spool)

(defparameter *library-root*
  (pathname (or (uiop:getenv "SPOOL_ROOT") "/mnt/lisp/spool/"))
  "Where subscriptions and cached audio live.  Deliberately not under $HOME — see
the file header; episodes are big and this machine's root volume is not.")

(defun %root () (ensure-directories-exist *library-root*))
(defun %audio-dir () (ensure-directories-exist (merge-pathnames "audio/" (%root))))
(defun %library-file () (merge-pathnames "library.sexp" (%root)))

;;; ---- naming a cached file --------------------------------------------------

(defun %fnv1a (string)
  "64-bit FNV-1a.  Not for security — for a short, STABLE filename.  SXHASH would
do the job in one call and is not promised to be stable across images, and a cache
key that changes when SBCL is upgraded is a cache that silently re-downloads
everything one day."
  (let ((h 14695981039346656037))
    (loop for c across string
          do (setf h (logand (* (logxor h (logand (char-code c) #xff)) 1099511628211)
                             #xffffffffffffffff)))
    h))

(defun %slug (s &key (limit 48))
  "A filesystem-safe, human-recognisable fragment of S.  Cache files are named by
hash for correctness and by slug for the human who has to look in the directory."
  (let ((out (make-string-output-stream)) (prev-dash t) (n 0))
    (loop for c across s
          while (< n limit)
          do (cond ((or (alphanumericp c) (member c '(#\- #\_)))
                    (write-char (char-downcase c) out) (setf prev-dash nil) (incf n))
                   ((not prev-dash) (write-char #\- out) (setf prev-dash t) (incf n))))
    (string-trim "-" (get-output-stream-string out))))

(defun %extension-for (episode)
  "The file extension to cache under.  Content-type first (the server's claim about
what it is sending), URL suffix second — because enclosure URLs are routinely
tracking redirects ending in /play.mp3?token=... or in nothing at all."
  (let ((ty (feed:episode-content-type episode))
        (url (feed:episode-url episode)))
    (cond ((search "mpeg" ty) "mp3")
          ((or (search "mp4" ty) (search "m4a" ty) (search "aac" ty)) "m4a")
          ((search "ogg" ty) "ogg")
          ((search ".mp3" url) "mp3")
          ((search ".m4a" url) "m4a")
          (t "mp3"))))

(defun cache-path (episode)
  "Where EPISODE's audio is (or would be) on disk."
  (merge-pathnames (format nil "~(~16,'0x~)-~a.~a"
                           (%fnv1a (feed:episode-url episode))
                           (%slug (feed:episode-title episode))
                           (%extension-for episode))
                   (%audio-dir)))

(defun cached-p (episode)
  (let ((p (cache-path episode)))
    (and (probe-file p)
         ;; A zero-length or obviously truncated file is not a cache hit.  A partial
         ;; download that reports as cached is worse than no cache: it plays for
         ;; twenty seconds and stops, and looks like a decoder bug.
         (let ((len (with-open-file (s p :element-type '(unsigned-byte 8)) (file-length s))))
           (and (> len 65536)
                (or (zerop (feed:episode-bytes episode))
                    ;; Advertised lengths are often wrong by a little; only reject
                    ;; when the file is obviously short of the claim.
                    (> len (* 0.9 (feed:episode-bytes episode))))
                p)))))

;;; ---- the library -----------------------------------------------------------

(defstruct (library (:constructor %make-library))
  (feeds (make-hash-table :test #'equal))     ; feed url -> FEED
  (order '())                                 ; feed urls, in subscription order
  (positions (make-hash-table :test #'equal)) ; episode guid -> seconds played
  (finished (make-hash-table :test #'equal))  ; episode guid -> t
  (lock (sb-thread:make-mutex :name "spool-library")))

(defvar *library* nil "The process's library; LOAD-LIBRARY fills it on first use.")

(defun %episode->plist (e)
  (list :guid (feed:episode-guid e) :title (feed:episode-title e)
        :url (feed:episode-url e) :bytes (feed:episode-bytes e)
        :type (feed:episode-content-type e) :seconds (feed:episode-seconds e)
        :published (feed:episode-published e) :summary (feed:episode-summary e)
        :feed-url (feed:episode-feed-url e)))

(defun %plist->episode (p)
  (feed:make-episode :guid (getf p :guid "") :title (getf p :title "")
                     :url (getf p :url "") :bytes (getf p :bytes 0)
                     :content-type (getf p :type "") :seconds (getf p :seconds)
                     :published (getf p :published) :summary (getf p :summary "")
                     :feed-url (getf p :feed-url "")))

(defun %feed->plist (f)
  (list :title (feed:feed-title f) :url (feed:feed-url f) :link (feed:feed-link f)
        :description (feed:feed-description f) :image (feed:feed-image f)
        :fetched-at (feed:feed-fetched-at f)
        :episodes (mapcar #'%episode->plist (feed:feed-episodes f))))

(defun %plist->feed (p)
  (feed:make-feed :title (getf p :title "") :url (getf p :url "")
                  :link (getf p :link "") :description (getf p :description "")
                  :image (getf p :image) :fetched-at (getf p :fetched-at 0)
                  :episodes (mapcar #'%plist->episode (getf p :episodes))))

(defun save-library (&optional (lib *library*))
  "Persist LIB.  Written to a temporary file and renamed over the real one, so an
interrupted save cannot destroy the subscription list."
  (sb-thread:with-mutex ((library-lock lib))
    (let* ((final (%library-file))
           (tmp (merge-pathnames (format nil "library-~d.tmp" (get-universal-time)) (%root))))
      (with-open-file (s tmp :direction :output :if-exists :supersede
                             :external-format :utf-8)
        (let ((*print-readably* nil) (*print-pretty* nil) (*print-circle* nil))
          (prin1 (list :version 1
                       :order (library-order lib)
                       :feeds (loop for url in (library-order lib)
                                    for f = (gethash url (library-feeds lib))
                                    when f collect (%feed->plist f))
                       :positions (let (acc) (maphash (lambda (k v) (push (cons k v) acc))
                                                      (library-positions lib))
                                    acc)
                       :finished (let (acc) (maphash (lambda (k v) (declare (ignore v)) (push k acc))
                                                     (library-finished lib))
                                   acc))
                 s)
          (terpri s)))
      (rename-file tmp final)
      final)))

(defun load-library ()
  "Read the library from disk, or start an empty one.  A corrupt file is reported
and stepped over rather than thrown: losing the subscription list is bad, but
refusing to start because of it is worse."
  (let ((lib (%make-library)))
    (handler-case
        (let ((path (%library-file)))
          (when (probe-file path)
            (let ((data (with-open-file (s path :external-format :utf-8)
                          (let ((*read-eval* nil)) (read s nil nil)))))
              (when data
                (setf (library-order lib) (getf data :order))
                (dolist (fp (getf data :feeds))
                  (let ((f (%plist->feed fp)))
                    (setf (gethash (feed:feed-url f) (library-feeds lib)) f)))
                (dolist (kv (getf data :positions))
                  (setf (gethash (car kv) (library-positions lib)) (cdr kv)))
                (dolist (g (getf data :finished))
                  (setf (gethash g (library-finished lib)) t))))))
      (error (e) (format *error-output* "~&[spool] library unreadable (~a) — starting empty~%" e)))
    (setf *library* lib)))

(defun library ()
  (or *library* (load-library)))

;;; ---- subscriptions ---------------------------------------------------------

(defun %fetch-feed (url)
  (let* ((r (weft.fetch:fetch url))
         (status (weft.fetch:response-status r)))
    ;; Terse on purpose: every caller already knows which URL it asked for, and says
    ;; so when it reports.  A message that repeats the URL costs the one line the app
    ;; has to say it in.
    (unless (= status 200)
      (error "HTTP ~a" status))
    (feed:parse-feed (weft.fetch:body-text (weft.fetch:response-headers r)
                                           (weft.fetch:response-body r))
                     ;; The FINAL url after redirects: a feed that moved should be
                     ;; remembered where it actually lives, or every refresh pays
                     ;; for the redirect again.
                     :url (weft.fetch:response-url r))))

(defun subscriptions (&optional (lib (library)))
  "The subscribed feeds, in subscription order."
  (loop for url in (library-order lib)
        for f = (gethash url (library-feeds lib))
        when f collect f))

(defun subscribe (url &optional (lib (library)))
  "Fetch URL, add it to the library, and return the FEED."
  (let ((f (%fetch-feed url)))
    (sb-thread:with-mutex ((library-lock lib))
      (let ((key (feed:feed-url f)))
        (setf (gethash key (library-feeds lib)) f)
        (pushnew key (library-order lib) :test #'string=)
        (setf (library-order lib) (append (remove key (library-order lib) :test #'string=)
                                          (list key)))))
    (save-library lib)
    f))

(defun unsubscribe (url &optional (lib (library)))
  (sb-thread:with-mutex ((library-lock lib))
    (remhash url (library-feeds lib))
    (setf (library-order lib) (remove url (library-order lib) :test #'string=)))
  (save-library lib)
  t)

(defun refresh (url &optional (lib (library)))
  "Re-fetch one feed, keeping it in place in the subscription order."
  (let ((f (%fetch-feed url)))
    (sb-thread:with-mutex ((library-lock lib))
      (setf (gethash url (library-feeds lib)) f)
      ;; A feed that redirected somewhere new is stored under BOTH keys: the order
      ;; list still names the old URL, and rewriting it here would reorder the
      ;; user's list as a side effect of a refresh.
      (unless (string= url (feed:feed-url f))
        (setf (gethash (feed:feed-url f) (library-feeds lib)) f)))
    (save-library lib)
    f))

(defun refresh-all (&optional (lib (library)))
  "Refresh every subscription.  Returns (values refreshed failures) — failures as
(url . condition), because one dead feed must not abort the other nine."
  (let ((ok 0) (bad '()))
    (dolist (url (copy-list (library-order lib)))
      (handler-case (progn (refresh url lib) (incf ok))
        (error (e) (push (cons url e) bad))))
    (values ok (nreverse bad))))

(defun episodes (&optional feed-or-url (lib (library)))
  "Episodes of one feed, or of everything, newest first."
  (cond
    ((feed:feed-p feed-or-url) (feed:feed-episodes feed-or-url))
    ((stringp feed-or-url)
     (let ((f (gethash feed-or-url (library-feeds lib)))) (and f (feed:feed-episodes f))))
    (t (let ((all (loop for f in (subscriptions lib) append (feed:feed-episodes f))))
         (append (sort (remove-if-not #'feed:episode-published all) #'>
                       :key #'feed:episode-published)
                 (remove-if #'feed:episode-published all))))))

(defun find-episode (guid &optional (lib (library)))
  (loop for f in (subscriptions lib)
        do (let ((hit (find guid (feed:feed-episodes f) :key #'feed:episode-guid :test #'string=)))
             (when hit (return hit)))))

;;; ---- where you got to ------------------------------------------------------

(defun position-of (episode &optional (lib (library)))
  "Seconds into EPISODE, or 0."
  (gethash (feed:episode-guid episode) (library-positions lib) 0))

(defun mark-position (episode seconds &key (save nil) (lib (library)))
  "Remember how far into EPISODE we are.  SAVE is off by default because this is
called on a timer while playing, and rewriting the whole library every few seconds
to record a position is not a trade worth making; the caller saves on pause, on
stop, and on quit."
  (setf (gethash (feed:episode-guid episode) (library-positions lib)) seconds)
  (when save (save-library lib))
  seconds)

(defun mark-played (episode &key (played t) (lib (library)))
  (if played
      (setf (gethash (feed:episode-guid episode) (library-finished lib)) t)
      (remhash (feed:episode-guid episode) (library-finished lib)))
  (save-library lib)
  played)

(defun played-p (episode &optional (lib (library)))
  (gethash (feed:episode-guid episode) (library-finished lib)))

;;; ---- the audio cache -------------------------------------------------------

(defvar *downloads* (make-hash-table :test #'equal)
  "guid -> (fetched . total) while a download is in flight.")
(defvar *downloads-lock* (sb-thread:make-mutex :name "spool-downloads"))

(defun downloading-p (episode)
  (sb-thread:with-mutex (*downloads-lock*)
    (nth-value 1 (gethash (feed:episode-guid episode) *downloads*))))

(defun download-progress (episode)
  "(values fetched total) for an in-flight download, or NIL."
  (sb-thread:with-mutex (*downloads-lock*)
    (let ((p (gethash (feed:episode-guid episode) *downloads*)))
      (when p (values (car p) (cdr p))))))

(defun download (episode &key force)
  "Fetch EPISODE's audio into the cache and return its path.  A cache hit returns
immediately.  The file is written under a temporary name and renamed, so an
interrupted download never leaves something that looks playable."
  (let ((have (unless force (cached-p episode))))
    (or have
        (let* ((path (cache-path episode))
               (tmp (make-pathname :type "part" :defaults path))
               (guid (feed:episode-guid episode)))
          (sb-thread:with-mutex (*downloads-lock*)
            (setf (gethash guid *downloads*) (cons 0 (feed:episode-bytes episode))))
          (unwind-protect
               (let* ((r (weft.fetch:fetch (feed:episode-url episode)))
                      (body (weft.fetch:response-body r)))
                 (unless (= 200 (weft.fetch:response-status r))
                   (error "~a returned HTTP ~a" (feed:episode-url episode)
                          (weft.fetch:response-status r)))
                 (with-open-file (s tmp :direction :output :element-type '(unsigned-byte 8)
                                        :if-exists :supersede)
                   (write-sequence body s))
                 (rename-file tmp path)
                 path)
            (sb-thread:with-mutex (*downloads-lock*) (remhash guid *downloads*))
            (ignore-errors (when (probe-file tmp) (delete-file tmp))))))))

(defun download-async (episode &key on-done on-error)
  "DOWNLOAD on its own thread.  Returns the thread.  A UI must never call DOWNLOAD
directly: a 186 MB episode takes long enough that a synchronous fetch would freeze
the window, and a frozen window on a desktop being driven over VNC from a phone
looks exactly like a crash."
  (sb-thread:make-thread
   (lambda ()
     (handler-case
         (let ((p (download episode))) (when on-done (funcall on-done p)))
       (serious-condition (e) (if on-error (funcall on-error e)
                                  (format *error-output* "~&[spool] download failed: ~a~%" e)))))
   :name (format nil "spool-dl ~a" (%slug (feed:episode-title episode) :limit 24))))

(defun cache-size ()
  "(values bytes files) currently in the audio cache."
  (let ((bytes 0) (n 0))
    (dolist (p (directory (merge-pathnames "*.*" (%audio-dir))) (values bytes n))
      (handler-case
          (with-open-file (s p :element-type '(unsigned-byte 8))
            (incf bytes (file-length s)) (incf n))
        (error () nil)))))

(defun evict (&key (keep-bytes (* 8 1024 1024 1024)))
  "Trim the cache to KEEP-BYTES, oldest-accessed first.  Returns bytes freed.

Eviction is by file mtime rather than by anything the library knows, on purpose:
the cache is allowed to contain files the library has forgotten about (an
unsubscribed show), and those should go first without needing a record of them."
  (let* ((files (sort (remove-if-not #'probe-file (directory (merge-pathnames "*.*" (%audio-dir))))
                      #'< :key (lambda (p) (or (ignore-errors (file-write-date p)) 0))))
         (total (nth-value 0 (cache-size)))
         (freed 0))
    (dolist (p files freed)
      (when (<= total keep-bytes) (return freed))
      (handler-case
          (let ((len (with-open-file (s p :element-type '(unsigned-byte 8)) (file-length s))))
            (delete-file p)
            (decf total len) (incf freed len))
        (error () nil)))))
