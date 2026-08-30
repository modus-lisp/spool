;;;; packages.lisp — spool's packages.
;;;;
;;;; Split by what each layer knows about.  SPOOL.XML knows about angle brackets and
;;;; nothing else; SPOOL.FEED knows what a podcast feed means and nothing about where
;;;; the bytes came from; SPOOL knows about the library on disk and playback.  The
;;;; CLIM app lives in SPOOL.APP and is the only package that has heard of a screen.

(defpackage #:spool.xml
  (:use #:cl)
  (:export #:parse-xml #:element #:element-p #:element-name #:element-attrs
           #:element-children #:elt-text #:attr #:child #:children #:find-path
           #:xml-error))

(defpackage #:spool.feed
  (:use #:cl)
  (:local-nicknames (#:xml #:spool.xml))
  (:export #:feed #:feed-p #:make-feed #:feed-title #:feed-url #:feed-link
           #:feed-description #:feed-image #:feed-episodes #:feed-fetched-at
           #:episode #:episode-p #:make-episode #:episode-guid #:episode-title
           #:episode-url #:episode-bytes #:episode-content-type #:episode-seconds
           #:episode-published #:episode-summary #:episode-feed-url
           #:parse-feed #:parse-duration #:format-duration))

(defpackage #:spool
  (:use #:cl)
  (:local-nicknames (#:feed #:spool.feed) (#:xml #:spool.xml))
  (:export ;; library
           #:*library-root* #:library #:load-library #:save-library
           #:subscriptions #:subscribe #:unsubscribe #:refresh #:refresh-all
           #:episodes #:find-episode #:position-of #:mark-position #:mark-played
           #:played-p
           ;; media
           #:cache-path #:cached-p #:download #:download-async #:downloading-p
           #:download-progress #:cache-size #:evict
           ;; playback
           #:player #:make-player #:player-p #:player-source #:player-episode #:player-title
           #:play-file
           #:player-state #:player-position #:player-duration #:player-error
           #:play #:pause #:resume #:toggle #:stop #:seek #:skip #:next-episode
           #:player-report))

(defpackage #:spool.glass
  (:use #:cl)
  (:local-nicknames (#:feed #:spool.feed))
  (:export #:*mixer* #:*player* #:attach #:detach #:ensure-playing #:report))
