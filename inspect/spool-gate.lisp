;;;; inspect/spool-gate.lisp — the unit gate: everything below the screen.
;;;;
;;;;   sbcl --control-stack-size 256 --dynamic-space-size 4096 --load inspect/spool-gate.lisp
;;;;
;;;; INSPECT/APP-DRIVE.LISP drives the CLIM client with a synthetic pointer and needs
;;;; a real subscription, a real cached episode and about a minute.  This gate needs
;;;; nothing: no network, no audio, no library on disk (it works in a temporary root
;;;; and puts it back), so it can run on every edit.  Between them the split is
;;;; "does it mean the right thing" here and "can you touch it" there.
;;;;
;;;; The fixtures are not tidy feeds.  Every shape in inspect/fixtures/rss.xml is one
;;;; a real show emitted at some point — CDATA around markup, numeric character
;;;; references, a namespaced duration, an item with no enclosure, an item with two,
;;;; an item with neither guid nor date, a DOCTYPE and a processing instruction before
;;;; the root.  A parser is only as good as the ugliest document it is asked to read.

(require :asdf)
(load "~/quicklisp/setup.lisp")
(handler-bind ((warning #'muffle-warning))
  (let ((*standard-output* (make-broadcast-stream)))
    (asdf:load-system :spool)))

(defpackage #:spool-gate
  (:use #:cl)
  (:local-nicknames (#:xml #:spool.xml) (#:feed #:spool.feed)))
(in-package #:spool-gate)

(defvar *pass* 0)
(defvar *fail* 0)
(defvar *group* "")

(defun ok (name got &optional (expected t) (test #'equal))
  (if (funcall test expected got)
      (incf *pass*)
      (progn (incf *fail*)
             (format t "~&  FAIL ~a / ~a~%       got ~s~%    wanted ~s~%" *group* name got expected))))

(defmacro group (name &body body)
  `(let ((*group* ,name) (before *fail*))
     ,@body
     (format t "~&  ~a ~a~%" (if (= before *fail*) "ok  " "FAIL") ,name)))

(defun fixture (name)
  (let ((path (merge-pathnames (format nil "fixtures/~a" name)
                               (directory-namestring *load-truename*))))
    (with-open-file (s path :external-format :utf-8)
      (let ((buf (make-string (file-length s))))
        (subseq buf 0 (read-sequence buf s))))))

;;; ---- XML -------------------------------------------------------------------

(group "xml: text, entities and CDATA"
  (let ((r (xml:parse-xml "<a>plain &amp; simple &lt;b&gt; &#65;&#x42; &nbsp; &notanentity; AT&T</a>")))
    (ok "entities resolve" (xml:elt-text r)
        (format nil "plain & simple <b> AB ~a &notanentity; AT&T" #\No-break_space)))
  (ok "CDATA is text, not markup"
      (xml:elt-text (xml:parse-xml "<a><![CDATA[<b>raw</b> & <]]></a>"))
      "<b>raw</b> & <")
  (ok "a CDATA section does not end at the first ]"
      (xml:elt-text (xml:parse-xml "<a><![CDATA[x] y]] z]]></a>")) "x] y]] z")
  ;; The reason this parser exists at all: <link> is a VOID element in HTML, so an
  ;; HTML parser reads the URL below as the item's own text and loses it.
  (let ((item (xml:parse-xml "<item><link>https://example.com/ep1</link><title>T</title></item>")))
    (ok "<link> has children here" (xml:elt-text (xml:child item "link"))
        "https://example.com/ep1")
    (ok "and its sibling survives" (xml:elt-text (xml:child item "title")) "T")))

(group "xml: structure"
  (let ((r (xml:parse-xml "<?xml version='1.0'?><!DOCTYPE x><!-- hi --><?pi go?><r a='1' b=\"2\"><c/><c>3</c></r>")))
    (ok "prologue junk is skipped" (xml:element-name r) "r")
    (ok "single quotes" (xml:attr r "a") "1")
    (ok "double quotes" (xml:attr r "b") "2")
    (ok "attr default" (xml:attr r "zz" :none) :none)
    (ok "self-closing counts" (length (xml:children r "c")) 2)
    (ok "children by name" (xml:elt-text (second (xml:children r "c"))) "3"))
  (ok "namespace prefixes match on local name"
      (xml:elt-text (xml:child (xml:parse-xml "<i><itunes:duration>90</itunes:duration></i>")
                               "duration"))
      "90")
  (ok "find-path walks" (xml:elt-text (xml:find-path (xml:parse-xml "<a><b><c>x</c></b></a>") "b" "c"))
      "x")
  (ok "find-path misses cleanly"
      (xml:find-path (xml:parse-xml "<a><b/></a>") "b" "nope") nil)
  ;; Tolerance: a feed is something a stranger's CMS emitted.
  (ok "a stray close tag is dropped"
      (xml:elt-text (xml:parse-xml "<a>one</b>two</a>")) "onetwo")
  ;; Why that matters, in the shape it actually arrives in: an unbalanced </b> in a
  ;; summary must not end the item, or the enclosure after it is lost and a real
  ;; episode silently stops existing.
  (let ((item (xml:parse-xml
               "<item><description>bold</b> text</description><enclosure url=\"u\"/></item>")))
    (ok "and it does not eat the rest of the item"
        (xml:attr (xml:child item "enclosure") "url") "u")
    (ok "the text after it is kept" (xml:elt-text (xml:child item "description")) "bold text"))
  (ok "an unclosed element is closed by its parent"
      (xml:element-name (xml:parse-xml "<a><b>x</a>")) "a")
  (ok "however deep it was left open"
      (xml:elt-text (xml:find-path (xml:parse-xml "<a><b><c>x</a><d>y</d>") "b" "c")) "x")
  (ok "and the parent's own close is not consumed twice"
      (xml:elt-text (xml:parse-xml "<a><b>x</a>")) "x")
  (ok "no document at all is the one error"
      (handler-case (progn (xml:parse-xml "   ") :no-error)
        (xml:xml-error () :signalled))
      :signalled))

;;; ---- durations and dates ---------------------------------------------------

(group "feed: durations"
  (ok "SS" (feed:parse-duration "90") 90)
  (ok "MM:SS" (feed:parse-duration "41:55") 2515)
  (ok "HH:MM:SS" (feed:parse-duration "1:02:03") 3723)
  (ok "surrounding space" (feed:parse-duration "  42 ") 42)
  (dolist (junk '("" "abc" "1:2:3:4" "12:" "-5" nil))
    (ok (format nil "~s is not a duration" junk) (feed:parse-duration junk) nil))
  (ok "nothing formats as nothing" (feed:format-duration nil) "--:--")
  (ok "under an hour" (feed:format-duration 62) "1:02")
  (ok "over an hour" (feed:format-duration 3723) "1:02:03")
  (ok "rounds, not truncates" (feed:format-duration 61.6) "1:02")
  (ok "zero" (feed:format-duration 0) "0:00"))

(group "feed: dates"
  (let ((noon (encode-universal-time 0 30 14 5 8 2026 0)))
    (ok "RFC 822" (feed::parse-date "Wed, 05 Aug 2026 14:30:00 +0000") noon)
    (ok "RFC 822 without the day name" (feed::parse-date "05 Aug 2026 14:30:00 GMT") noon)
    (ok "RFC 822 in another zone" (feed::parse-date "Wed, 05 Aug 2026 09:30:00 -0500") noon)
    (ok "ISO 8601 Z" (feed::parse-date "2026-08-05T14:30:00Z") noon)
    (ok "ISO 8601 offset" (feed::parse-date "2026-08-05T16:30:00+02:00") noon)
    (ok "ISO 8601 fractional seconds" (feed::parse-date "2026-08-05T14:30:00.123Z") noon))
  ;; A wrong date sorts an episode list wrongly and silently; NIL is the honest answer.
  (dolist (junk '("" "next tuesday" "2026" "Wed, 05 Foo 2026 14:30:00 +0000" nil))
    (ok (format nil "~s is not a date" junk) (feed::parse-date junk) nil)))

;;; ---- a whole feed ----------------------------------------------------------

(defvar *rss* (feed:parse-feed (fixture "rss.xml") :url "https://example.com/feed.xml"))
(defvar *atom* (feed:parse-feed (fixture "atom.xml") :url "https://atom.example.com/feed.xml"))

(group "feed: RSS 2.0"
  (ok "title, entity and all" (feed:feed-title *rss*) "Ghosts & Machines")
  (ok "the show's page" (feed:feed-link *rss*) "https://example.com/show")
  (ok "description out of CDATA" (feed:feed-description *rss*)
      "A show about <b>things</b> & other things.")
  (ok "artwork" (feed:feed-image *rss*) "https://example.com/art.jpg")
  (ok "the url we fetched it from is remembered" (feed:feed-url *rss*)
      "https://example.com/feed.xml")
  (let ((eps (feed:feed-episodes *rss*)))
    (ok "an item with no enclosure is not an episode" (length eps) 4)
    (ok "newest first, undated last" (mapcar #'feed:episode-title eps)
        (list (format nil "The world~as oldest bug" #\Right_single_quotation_mark)
              "Second oldest" "Third oldest" "Undated & unidentified"))
    (let ((e (first eps)))
      (ok "guid" (feed:episode-guid e) "tag:example.com,2026:ep-3")
      (ok "enclosure url" (feed:episode-url e) "https://cdn.example.com/ep3.mp3")
      (ok "advertised length" (feed:episode-bytes e) 9000000)
      (ok "content type" (feed:episode-content-type e) "audio/mpeg")
      (ok "namespaced duration" (feed:episode-seconds e) 3723)
      ;; Verbatim, references and all: that is what CDATA MEANS, and the XML layer
      ;; is not allowed to know that this particular field will be read by a person.
      ;; If a summary ever reaches the screen, the unescaping belongs at that end —
      ;; decoding here would also decode the &amp;#8217; a publisher wrote on purpose.
      (ok "a CDATA summary is verbatim" (feed:episode-summary e)
          "Curly &#8216;quotes&#8217; and an &nbsp; entity.")
      (ok "which show it came from" (feed:episode-feed-url e) "https://example.com/feed.xml"))
    (ok "audio beats video when a feed offers both"
        (feed:episode-url (second eps)) "https://cdn.example.com/ep2.m4a")
    (ok "an episode with no guid is identified by its audio"
        (feed:episode-guid (fourth eps)) "https://cdn.example.com/ep0.mp3")
    (ok "an undated episode has no date, rather than a guessed one"
        (feed:episode-published (fourth eps)) nil)))

(group "feed: Atom"
  (ok "title" (feed:feed-title *atom*) "Atom Only")
  (ok "<subtitle> is the description" (feed:feed-description *atom*)
      "The other dialect, which real shows do use.")
  (ok "rel=alternate is the page, not rel=self" (feed:feed-link *atom*)
      "https://atom.example.com/")
  (let ((eps (feed:feed-episodes *atom*)))
    (ok "entries are episodes" (length eps) 2)
    (ok "newest first" (mapcar #'feed:episode-title eps) '("Newer entry" "Older entry"))
    (ok "<id> is the guid" (feed:episode-guid (first eps)) "urn:uuid:0000-0002")
    (ok "rel=enclosure is the audio" (feed:episode-url (first eps))
        "https://cdn.atom.example.com/2.mp3")
    (ok "<summary> is the summary" (feed:episode-summary (first eps))
        "The summary tag, not description.")
    (ok "<updated> stands in for <published>"
        (feed:episode-published (second eps)) (encode-universal-time 0 0 12 1 7 2026 0))
    (ok "a duration the feed never gave is NIL, not zero"
        (feed:episode-seconds (first eps)) nil)))

;;; ---- the library on disk ---------------------------------------------------

(defun temp-root ()
  (pathname (format nil "/tmp/spool-gate-~d/" (get-universal-time))))

(group "library: naming the cache"
  (let* ((e (first (feed:feed-episodes *rss*)))
         (p (spool:cache-path e)))
    (ok "named by hash and slug" (pathname-name p)
        "b8a5e0e0d4a5b9c1-the-world-s-oldest-bug"
        (lambda (want got) (declare (ignore want))
          (and (= 16 (position #\- got)) (search "the-world-s-oldest-bug" got) t)))
    (ok "audio/mpeg is .mp3" (pathname-type p) "mp3")
    (ok "the name is stable across calls" (namestring (spool:cache-path e)) (namestring p))
    (ok "audio/mp4 is .m4a" (pathname-type (spool:cache-path (second (feed:feed-episodes *rss*))))
        "m4a")
    ;; A tracking URL ending in nothing at all still has to land somewhere.
    (ok "an untyped tracking url falls back to mp3"
        (pathname-type (spool:cache-path (third (feed:feed-episodes *rss*)))) "mp3")
    (ok "a file that is not there is not cached" (spool:cached-p e) nil)))

(group "library: round trip"
  (let* ((root (temp-root))
         (spool:*library-root* root)
         (lib (spool::%make-library))
         (e (first (feed:feed-episodes *rss*))))
    (unwind-protect
         (progn
           (setf (gethash (feed:feed-url *rss*) (spool::library-feeds lib)) *rss*
                 (gethash (feed:feed-url *atom*) (spool::library-feeds lib)) *atom*
                 (spool::library-order lib) (list (feed:feed-url *rss*) (feed:feed-url *atom*)))
           (spool:mark-position e 61.5d0 :lib lib)
           (spool:mark-played (second (feed:feed-episodes *rss*)) :lib lib)
           (spool:save-library lib)
           (ok "the file is where it should be"
               (and (probe-file (spool::%library-file)) t) t)
           (let ((spool::*library* nil))
             (let ((back (spool:load-library)))
               (ok "subscriptions come back in order"
                   (mapcar #'feed:feed-title (spool:subscriptions back))
                   '("Ghosts & Machines" "Atom Only"))
               (ok "episodes come back" (length (spool:episodes (feed:feed-url *rss*) back)) 4)
               (ok "titles survive the trip"
                   (feed:episode-title (first (spool:episodes (feed:feed-url *rss*) back)))
                   (feed:episode-title e))
               (ok "a saved position is where you left it" (spool:position-of e back) 61.5d0)
               (ok "an unheard episode is at zero"
                   (spool:position-of (fourth (feed:feed-episodes *rss*)) back) 0)
               (ok "played stays played"
                   (and (spool:played-p (second (feed:feed-episodes *rss*)) back) t) t)
               (ok "find-episode finds it by guid"
                   (feed:episode-title (spool:find-episode "urn:uuid:0000-0001" back))
                   "Older entry")
               (ok "everything, newest first, across shows"
                   (mapcar #'feed:episode-title (spool:episodes nil back))
                   (list (format nil "The world~as oldest bug" #\Right_single_quotation_mark)
                         "Second oldest" "Third oldest" "Newer entry" "Older entry"
                         "Undated & unidentified"))))
           ;; Losing the subscription list is bad; refusing to start is worse.
           (with-open-file (s (spool::%library-file) :direction :output :if-exists :supersede)
             (write-string "(:version 1 :order (\"x\"" s))
           (let ((spool::*library* nil)
                 (*error-output* (make-broadcast-stream)))
             (ok "a truncated library starts empty instead of refusing to start"
                 (spool:subscriptions (spool:load-library)) '())))
      (ignore-errors (uiop:delete-directory-tree root :validate t)))))

;;; ---- the player ------------------------------------------------------------

(group "player: the contract a mixer relies on"
  (let ((p (spool:make-player)))
    (ok "an idle player is idle" (spool:player-state p) :idle)
    (ok "and hands out silence rather than stalling"
        (loop repeat 5 always (null (funcall (spool:player-source p)))) t)
    (ok "the same thunk is good forever" (functionp (spool:player-source p)) t)
    (ok "nothing playing is position zero" (spool:player-position p) 0.0d0)
    ;; PLAY does not download: minutes-long work does not hide inside a call the UI
    ;; expects to return.
    (let ((spool::*library* (spool::%make-library)))
      (ok "playing what is not on disk is an error state, not a condition"
          (spool:play p (first (feed:feed-episodes *rss*))) nil)
      (ok "and it says why" (spool:player-error p) "not downloaded")
      (ok "state" (spool:player-state p) :error))))

(group "player: position arithmetic"
  (let ((p (spool:make-player :rate 48000)))
    (setf (spool::player-emitted p) 48000)
    (ok "a second of samples is a second" (spool:player-position p) 1.0d0)
    (setf (spool::player-base p) 100.0d0 (spool::player-emitted p) 24000)
    (ok "position is measured from the last seek" (spool:player-position p) 100.5d0)
    (setf (spool::player-duration p) 3723 (spool::player-episode p) nil)
    (ok "the report is one line" (spool:player-report p) "idle 1:40/1:02:03"))
  ;; SEEK needs a known duration: without one there is no byte offset to compute,
  ;; and inventing a duration scrubs to a random place in the show.
  (let ((p (spool:make-player)))
    (ok "seeking a player with no decoder does nothing" (spool:seek p 30) nil)
    (setf (spool::player-duration p) nil)
    (ok "and neither does skipping" (spool:skip p 30) nil)))

(format t "~&~%== spool gate: ~d passed, ~d failed ==~%" *pass* *fail*)
(finish-output)
(sb-ext:exit :code (if (plusp *fail*) 1 0))
