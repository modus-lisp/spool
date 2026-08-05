;;;; feed.lisp — what a podcast feed means, once someone else has read the angle brackets.
;;;;
;;;; RSS 2.0 and Atom both, because both exist and a reader that handles one of them
;;;; is a reader that fails on a subscription the user pasted in good faith.  They
;;;; differ in nearly every name (<item>/<entry>, <pubDate>/<published>,
;;;; <description>/<summary>) but agree on the shape, so this file is mostly a
;;;; translation table with one real algorithm in it (duration parsing).
;;;;
;;;; The one field that MUST be right is the enclosure URL: everything else is
;;;; decoration, and an episode without audio is not an episode.  So EPISODES drops
;;;; entries with no enclosure rather than carrying half-episodes the UI would have
;;;; to keep re-checking.
;;;;
;;;; What an episode is identified BY is the guid, falling back to the enclosure URL.
;;;; Not the title: shows reuse titles ("Q&A"), and not the position in the feed,
;;;; which changes every time the show publishes.  The guid is what lets a saved
;;;; playback position still mean something after a refresh.

(in-package #:spool.feed)

(defstruct feed
  (title "" :type string)
  (url "" :type string)                 ; where the feed itself lives
  (link "")                             ; the show's web page
  (description "")
  (image nil)
  (episodes '())
  (fetched-at 0))

(defstruct episode
  (guid "" :type string)
  (title "" :type string)
  (url "" :type string)                 ; the enclosure — the audio itself
  (bytes 0)                             ; as advertised; often a lie, never trusted
  (content-type "")
  (seconds nil)                         ; as advertised; NIL when the feed omits it
  (published nil)                       ; universal time, or NIL if unparseable
  (summary "")
  (feed-url ""))

;;; ---- durations -------------------------------------------------------------

(defun parse-duration (s)
  "<itunes:duration> as seconds.  Accepts SS, MM:SS and HH:MM:SS, which is the whole
zoo in practice — the tag is specified as one of those three and honoured as
whichever the publisher's CMS felt like.  NIL if it is none of them."
  (when (and s (plusp (length s)))
    (let ((s (string-trim '(#\Space #\Tab) s)))
      (handler-case
          (let ((parts (loop with start = 0
                             for c = (position #\: s :start start)
                             collect (subseq s start c)
                             while c do (setf start (1+ c)))))
            (when (every (lambda (p) (and (plusp (length p)) (every #'digit-char-p p))) parts)
              (let ((nums (mapcar #'parse-integer parts)))
                (case (length nums)
                  (1 (first nums))
                  (2 (+ (* 60 (first nums)) (second nums)))
                  (3 (+ (* 3600 (first nums)) (* 60 (second nums)) (third nums)))
                  (t nil)))))
        (error () nil)))))

(defun format-duration (secs)
  "Seconds as H:MM:SS or M:SS — for a UI, so no leading zero on the first field."
  (if (null secs)
      "--:--"
      (multiple-value-bind (h rem) (floor (round secs) 3600)
        (multiple-value-bind (m s) (floor rem 60)
          (if (plusp h) (format nil "~d:~2,'0d:~2,'0d" h m s) (format nil "~d:~2,'0d" m s))))))

;;; ---- dates -----------------------------------------------------------------

(defparameter +months+
  '(("jan" . 1) ("feb" . 2) ("mar" . 3) ("apr" . 4) ("may" . 5) ("jun" . 6)
    ("jul" . 7) ("aug" . 8) ("sep" . 9) ("oct" . 10) ("nov" . 11) ("dec" . 12)))

(defun %split (s &optional (seps '(#\Space #\Tab #\Newline #\Return)))
  (let ((out '()) (cur (make-string-output-stream)))
    (loop for c across s
          do (if (member c seps)
                 (let ((tok (get-output-stream-string cur)))
                   (when (plusp (length tok)) (push tok out)))
                 (write-char c cur)))
    (let ((tok (get-output-stream-string cur)))
      (when (plusp (length tok)) (push tok out)))
    (nreverse out)))

(defun parse-date (s)
  "RFC 822 (RSS <pubDate>) or ISO 8601 (Atom <published>) as a universal time.

Returns NIL rather than guessing when it cannot tell.  A wrong date sorts an
episode list wrongly and silently, which is worse than an absent one: the UI can
say nothing, but it cannot un-believe a date."
  (when (and s (plusp (length s)))
    (handler-case
        (let ((s (string-trim '(#\Space #\Tab #\Newline #\Return) s)))
          (cond
            ;; ISO 8601: 2026-08-05T14:30:00Z / +01:00 / with fractional seconds
            ((and (> (length s) 9) (digit-char-p (char s 0)) (char= (char s 4) #\-))
             (let* ((y (parse-integer s :start 0 :end 4))
                    (mo (parse-integer s :start 5 :end 7))
                    (d (parse-integer s :start 8 :end 10))
                    (hh 0) (mm 0) (ss 0) (zone 0))
               (when (and (> (length s) 15) (member (char s 10) '(#\T #\t #\Space)))
                 (setf hh (parse-integer s :start 11 :end 13)
                       mm (parse-integer s :start 14 :end 16))
                 (when (and (> (length s) 18) (char= (char s 16) #\:))
                   (setf ss (parse-integer s :start 17 :end 19)))
                 (let ((sign (position-if (lambda (c) (member c '(#\+ #\-))) s :start 19)))
                   (when sign
                     (let ((oh (parse-integer s :start (1+ sign) :end (+ sign 3))))
                       ;; CL zones are hours WEST of UTC, so the sign inverts.
                       (setf zone (if (char= (char s sign) #\+) (- oh) oh))))))
               (encode-universal-time ss mm hh d mo y zone)))
            ;; RFC 822: Wed, 05 Aug 2026 14:30:00 +0000
            (t
             (let* ((toks (%split (substitute #\Space #\, s)))
                    (toks (if (and toks (not (digit-char-p (char (first toks) 0))))
                              (rest toks)   ; drop the day name
                              toks)))
               (when (>= (length toks) 4)
                 (let* ((d (parse-integer (first toks)))
                        (mo (cdr (assoc (subseq (second toks) 0 (min 3 (length (second toks))))
                                        +months+ :test #'string-equal)))
                        (y (parse-integer (third toks)))
                        (hms (%split (fourth toks) '(#\:)))
                        (hh (if hms (parse-integer (first hms)) 0))
                        (mm (if (cdr hms) (parse-integer (second hms)) 0))
                        (ss (if (cddr hms) (parse-integer (third hms)) 0))
                        (zs (fifth toks))
                        (zone (cond ((null zs) 0)
                                    ((member zs '("GMT" "UT" "UTC" "Z") :test #'string-equal) 0)
                                    ((member (char zs 0) '(#\+ #\-))
                                     (let ((oh (parse-integer zs :start 1 :end 3)))
                                       (if (char= (char zs 0) #\+) (- oh) oh)))
                                    (t 0))))
                   (when (and mo (<= 1 d 31) (> y 1900))
                     (encode-universal-time ss mm hh d mo y zone))))))))
      (error () nil))))

;;; ---- the feed itself -------------------------------------------------------

(defun %enclosure (item)
  "The audio for ITEM as (values url bytes type), across both dialects.

RSS says <enclosure url= length= type=/>.  Atom says <link rel=\"enclosure\"
href= length= type=/>.  Some feeds carry several enclosures (a video cut, a
transcript); prefer an audio/* one, else take the first, because a feed that only
offers video is still a thing a user can listen to."
  (let* ((rss (xml:children item "enclosure"))
         (atom (remove-if-not (lambda (l) (string-equal (xml:attr l "rel" "") "enclosure"))
                              (xml:children item "link")))
         (all (append rss atom))
         (audio (find-if (lambda (e) (let ((ty (xml:attr e "type" "")))
                                       (and ty (>= (length ty) 5) (string-equal "audio" ty :end2 5))))
                         all))
         (pick (or audio (first all))))
    (when pick
      (values (or (xml:attr pick "url") (xml:attr pick "href") "")
              (or (ignore-errors (parse-integer (xml:attr pick "length" "0"))) 0)
              (xml:attr pick "type" "")))))

(defun %item->episode (item feed-url)
  (multiple-value-bind (url bytes type) (%enclosure item)
    (when (and url (plusp (length url)))
      (make-episode
       :guid (let ((g (xml:elt-text (or (xml:child item "guid") (xml:child item "id")))))
               (if (and g (plusp (length g))) g url))
       :title (or (xml:elt-text (xml:child item "title")) "(untitled)")
       :url url :bytes bytes :content-type type
       :seconds (parse-duration (xml:elt-text (xml:child item "duration")))
       :published (parse-date (xml:elt-text (or (xml:child item "pubDate")
                                                (xml:child item "published")
                                                (xml:child item "updated"))))
       :summary (or (xml:elt-text (or (xml:child item "description")
                                      (xml:child item "summary")
                                      (xml:child item "subtitle")))
                    "")
       :feed-url feed-url))))

(defun parse-feed (string &key (url ""))
  "Parse a feed document into a FEED with its EPISODES.  URL is remembered, because
an episode needs to know which show it came from and the document does not
reliably say."
  (let* ((root (xml:parse-xml string))
         ;; RSS nests everything under <channel>; Atom puts it at the root.
         (chan (or (xml:child root "channel") root))
         (items (append (xml:children chan "item") (xml:children chan "entry")))
         (image (or (xml:attr (or (xml:child chan "image") chan) "href" nil)
                    (xml:elt-text (xml:find-path chan "image" "url")))))
    (make-feed
     :title (or (xml:elt-text (xml:child chan "title")) "(untitled feed)")
     :url url
     :link (or (let ((l (find-if (lambda (e) (or (null (xml:attr e "rel" nil))
                                                 (string-equal (xml:attr e "rel" "") "alternate")))
                                 (xml:children chan "link"))))
                 (and l (let ((h (xml:attr l "href" nil))) (or h (xml:elt-text l)))))
               "")
     :description (or (xml:elt-text (or (xml:child chan "description")
                                        (xml:child chan "subtitle")))
                      "")
     :image (and image (plusp (length image)) image)
     :fetched-at (get-universal-time)
     ;; Newest first.  Feeds are supposed to be in that order already and mostly
     ;; are; sorting by the date we parsed means one publisher's broken ordering
     ;; does not become the reader's.  Episodes with no date keep feed order at the
     ;; end, since NIL cannot be compared and inventing a date would be worse.
     :episodes (let* ((eps (remove nil (mapcar (lambda (i) (%item->episode i url)) items)))
                      (dated (remove-if-not #'episode-published eps))
                      (undated (remove-if #'episode-published eps)))
                 (append (sort dated #'> :key #'episode-published) undated)))))
