;;;; xml.lisp — enough XML to read a podcast feed, and deliberately not more.
;;;;
;;;; Why write this at all.  The obvious move is to point the HTML parser we already
;;;; have (weft.html) at a feed, and it is the wrong move: HTML5 parsing is not a
;;;; lenient superset of XML, it is a DIFFERENT grammar with a fixed element
;;;; vocabulary.  <link> is a VOID element in HTML — it never has children and never
;;;; closes — so `<link>https://example.com/ep1</link>` in an RSS item parses as an
;;;; empty <link>, a text node, and a stray close tag, and the episode's page URL
;;;; silently becomes part of its parent's text.  <title> is RCDATA, so a nested
;;;; feed structure flattens.  Neither failure looks like an error; both look like a
;;;; feed with missing fields.  A parser for the grammar the document actually claims
;;;; is smaller than the workarounds would have been.
;;;;
;;;; What this handles, because feeds in the wild contain it: elements, attributes
;;;; (single or double quoted), self-closing tags, comments, CDATA, processing
;;;; instructions, DOCTYPE, the five predefined entities, numeric character
;;;; references (decimal and hex), and namespace prefixes.
;;;;
;;;; Namespaces are kept as PREFIXES, not resolved to URIs.  A real XML processor
;;;; must resolve them; a feed reader must not care, because every feed in practice
;;;; binds itunes: to the iTunes namespace and content: to the content namespace, and
;;;; a reader that resolved URIs properly would still be looking those two prefixes
;;;; up by name.  Resolving would be more correct and buy nothing.  Where it WOULD
;;;; matter — a feed binding a prefix unusually — CHILD matches on local name too, so
;;;; "duration" finds <itunes:duration> regardless of what the prefix was called.
;;;;
;;;; Tolerance: this parser does not reject.  A stray close tag is dropped, an
;;;; unclosed element is closed by its parent, junk before the root is skipped.  A
;;;; feed is something a stranger's CMS emitted, and refusing to read a 300 KB
;;;; document over one bad byte in an episode summary serves nobody.  It signals
;;;; XML-ERROR only when there is no document at all.

(in-package #:spool.xml)

(define-condition xml-error (error)
  ((message :initarg :message :reader xml-error-message))
  (:report (lambda (c s) (format s "~a" (xml-error-message c)))))

(defstruct (element (:constructor %make-element))
  "One XML element.  NAME is as written, prefix included (\"itunes:duration\").
CHILDREN holds elements and strings interleaved, in document order — a feed's
<description> can legitimately contain both."
  (name "" :type string)
  (attrs '())                           ; alist of (name . value), names as written
  (children '()))

;;; ---- character data --------------------------------------------------------

(defparameter +entities+
  '(("amp" . #\&) ("lt" . #\<) ("gt" . #\>) ("quot" . #\") ("apos" . #\')
    ;; Not predefined in XML, but emitted by enough CMSes that treating it as an
    ;; error would mean losing text.  HTML's value; harmless anywhere else.
    ("nbsp" . #\No-break_space))
  "The entities a feed may use without declaring them.")

(defun %decode-entity (name)
  "&NAME; -> its character, or NIL if we do not know it (then it stays literal)."
  (cond
    ((zerop (length name)) nil)
    ((char= (char name 0) #\#)
     ;; Numeric reference.  Out-of-range or unparseable is not fatal: the caller
     ;; keeps the raw text, which is at worst ugly and at best still readable.
     (let* ((hex (and (> (length name) 1) (member (char name 1) '(#\x #\X))))
            (digits (subseq name (if hex 2 1)))
            (code (ignore-errors (parse-integer digits :radix (if hex 16 10)))))
       (when (and code (< 0 code char-code-limit)) (code-char code))))
    (t (cdr (assoc name +entities+ :test #'string-equal)))))

(defun unescape (s)
  "Resolve entity and character references in S.  Everything else passes through."
  (if (not (find #\& s))
      s
      (with-output-to-string (out)
        (let ((i 0) (n (length s)))
          (loop while (< i n) do
            (let ((c (char s i)))
              (if (char/= c #\&)
                  (progn (write-char c out) (incf i))
                  ;; A bare & is common in hand-written feeds.  Only treat it as a
                  ;; reference if a ; follows within a plausible distance.
                  (let ((semi (position #\; s :start i :end (min n (+ i 12)))))
                    (let ((ch (and semi (%decode-entity (subseq s (1+ i) semi)))))
                      (cond (ch (write-char ch out) (setf i (1+ semi)))
                            (t (write-char c out) (incf i))))))))))))

;;; ---- the scanner -----------------------------------------------------------

(defstruct (scanner (:conc-name sc-))
  (s "" :type string) (i 0 :type fixnum) (n 0 :type fixnum)
  ;; The names of the elements currently open, innermost first, and the name of a
  ;; close tag that belongs to one of them but was met deeper down.  Together they
  ;; are what lets a stray close tag be DROPPED (its name is open nowhere) while a
  ;; close tag for an ancestor still closes everything between here and it.
  (stack '()) (pending nil))

(declaim (inline sc-peek sc-eof))
(defun sc-eof (sc) (>= (sc-i sc) (sc-n sc)))
(defun sc-peek (sc &optional (off 0))
  (let ((k (+ (sc-i sc) off)))
    (when (< k (sc-n sc)) (char (sc-s sc) k))))

(defun sc-looking-at (sc str)
  (let ((end (+ (sc-i sc) (length str))))
    (and (<= end (sc-n sc)) (string= (sc-s sc) str :start1 (sc-i sc) :end1 end))))

(defun sc-skip-to (sc str)
  "Advance past the next occurrence of STR; to end of input if it never comes."
  (let ((at (search str (sc-s sc) :start2 (sc-i sc))))
    (setf (sc-i sc) (if at (+ at (length str)) (sc-n sc)))))

(defun sc-take-until (sc str)
  "Text up to the next STR (exclusive), consuming STR.  Unterminated -> the rest."
  (let* ((at (search str (sc-s sc) :start2 (sc-i sc)))
         (end (or at (sc-n sc)))
         (text (subseq (sc-s sc) (sc-i sc) end)))
    (setf (sc-i sc) (if at (+ at (length str)) (sc-n sc)))
    text))

(defun name-char-p (c)
  (or (alphanumericp c) (member c '(#\: #\- #\_ #\.))))

(defun sc-name (sc)
  (let ((start (sc-i sc)))
    (loop while (and (not (sc-eof sc)) (name-char-p (sc-peek sc))) do (incf (sc-i sc)))
    (subseq (sc-s sc) start (sc-i sc))))

(defun sc-skip-space (sc)
  (loop while (and (not (sc-eof sc))
                   (member (sc-peek sc) '(#\Space #\Tab #\Newline #\Return #\Page)))
        do (incf (sc-i sc))))

(defun sc-attrs (sc)
  "Attributes up to (not including) > or />."
  (let ((attrs '()))
    (loop
      (sc-skip-space sc)
      (when (or (sc-eof sc) (member (sc-peek sc) '(#\> #\/))) (return))
      (let ((name (sc-name sc)))
        (if (zerop (length name))
            ;; A name that consumed nothing means something we do not understand is
            ;; in the tag; step over one character so this cannot spin forever.
            (incf (sc-i sc))
            (progn
              (sc-skip-space sc)
              (let ((value ""))
                (when (eql (sc-peek sc) #\=)
                  (incf (sc-i sc))
                  (sc-skip-space sc)
                  (let ((q (sc-peek sc)))
                    (cond ((member q '(#\" #\'))
                           (incf (sc-i sc))
                           (setf value (unescape (sc-take-until sc (string q)))))
                          (t ;; unquoted value — not legal XML, common in the wild
                           (let ((start (sc-i sc)))
                             (loop while (and (not (sc-eof sc))
                                              (not (member (sc-peek sc)
                                                           '(#\Space #\Tab #\Newline
                                                             #\Return #\> #\/))))
                                   do (incf (sc-i sc)))
                             (setf value (unescape (subseq (sc-s sc) start (sc-i sc)))))))))
                (push (cons name value) attrs))))))
    (nreverse attrs)))

;;; ---- the parser ------------------------------------------------------------

(defun %same-name-p (a b)
  (or (string-equal a b) (string-equal (local-name a) (local-name b))))

(defun %parse-element (sc)
  "Parse one element, assuming the scanner sits just past its opening <."
  (let* ((name (sc-name sc))
         (attrs (sc-attrs sc))
         (children '()))
    (cond
      ((sc-looking-at sc "/>") (incf (sc-i sc) 2))
      ((eql (sc-peek sc) #\>)
       (incf (sc-i sc))
       (push name (sc-stack sc))
       (loop
         (when (sc-eof sc) (return))
         (cond
           ;; close tag
           ((sc-looking-at sc "</")
            (incf (sc-i sc) 2)
            (let ((cname (sc-name sc)))
              (sc-skip-to sc ">")
              (cond
                ;; Ours: done.
                ((%same-name-p cname name) (return))
                ;; An ancestor's: everything open between here and it was never
                ;; closed, so close it all — that is what the document meant.
                ((find cname (rest (sc-stack sc)) :test #'%same-name-p)
                 (setf (sc-pending sc) cname)
                 (return))
                ;; Nobody's: a stray close tag, of which feeds are full (an unbalanced
                ;; </b> inside a summary).  Dropping it keeps the REST of this element
                ;; — treating it as closing us instead would hand the element's tail
                ;; to our parent and, one level up, eat the enclosure.
                (t nil))))
           ((sc-looking-at sc "<!--") (sc-skip-to sc "-->"))
           ((sc-looking-at sc "<![CDATA[")
            (incf (sc-i sc) 9)
            ;; CDATA is verbatim: no entity decoding, which is the whole point of it.
            (let ((raw (sc-take-until sc "]]>")))
              (when (plusp (length raw)) (push raw children))))
           ((sc-looking-at sc "<?") (sc-skip-to sc "?>"))
           ((sc-looking-at sc "<!") (sc-skip-to sc ">"))
           ((eql (sc-peek sc) #\<)
            (incf (sc-i sc))
            (let ((child (%parse-element sc)))
              (when child (push child children)))
            ;; A close tag for one of OUR ancestors was met inside that child.  If it
            ;; was ours, it is spent here; if not, keep unwinding.
            (when (sc-pending sc)
              (when (%same-name-p (sc-pending sc) name) (setf (sc-pending sc) nil))
              (return)))
           (t (let ((text (sc-take-until-lt sc)))
                (when (plusp (length text)) (push (unescape text) children))))))
       (pop (sc-stack sc)))
      (t ;; a < that never became a tag
       nil))
    (%make-element :name name :attrs attrs :children (nreverse children))))

(defun sc-take-until-lt (sc)
  (let* ((at (position #\< (sc-s sc) :start (sc-i sc)))
         (end (or at (sc-n sc)))
         (text (subseq (sc-s sc) (sc-i sc) end)))
    (setf (sc-i sc) end)
    text))

(defun parse-xml (string)
  "Parse STRING and return its root ELEMENT.  Signals XML-ERROR if there is no
element in it at all — which is the one failure a caller genuinely must handle,
because it means what came back was not a feed (an HTML error page, usually)."
  (let ((sc (make-scanner :s string :i 0 :n (length string))))
    (loop
      (when (sc-eof sc)
        (error 'xml-error :message "no XML element found"))
      (cond
        ((sc-looking-at sc "<!--") (sc-skip-to sc "-->"))
        ((sc-looking-at sc "<?") (sc-skip-to sc "?>"))
        ((sc-looking-at sc "<!") (sc-skip-to sc ">"))
        ((and (eql (sc-peek sc) #\<) (let ((c (sc-peek sc 1))) (and c (name-char-p c))))
         (incf (sc-i sc))
         (return (%parse-element sc)))
        (t (incf (sc-i sc)))))))

;;; ---- reading the tree ------------------------------------------------------

(defun local-name (name)
  (let ((c (position #\: name))) (if c (subseq name (1+ c)) name)))

(defun name-matches-p (element name)
  "Match on the full name first, then on the local name — so \"itunes:duration\"
and \"duration\" both find the same element whatever prefix the feed chose."
  (let ((have (element-name element)))
    (or (string-equal have name)
        (string-equal (local-name have) (local-name name)))))

(defun child (element name)
  "The first child element of ELEMENT named NAME, or NIL."
  (find-if (lambda (c) (and (element-p c) (name-matches-p c name)))
           (element-children element)))

(defun children (element &optional name)
  "ELEMENT's child ELEMENTS, optionally only those named NAME."
  (remove-if-not (lambda (c) (and (element-p c) (or (null name) (name-matches-p c name))))
                 (element-children element)))

(defun find-path (element &rest names)
  "Walk NAMES down from ELEMENT: (find-path root \"channel\" \"title\")."
  (let ((at element))
    (dolist (n names at)
      (setf at (and at (child at n))))))

(defun attr (element name &optional default)
  (let ((hit (assoc name (element-attrs element)
                    :test (lambda (a b) (or (string-equal a b)
                                            (string-equal a (local-name b)))))))
    (if hit (cdr hit) default)))

(defun elt-text (element &key (trim t))
  "All text under ELEMENT, descendants included, concatenated.

Descendants included on purpose: feeds put markup inside <description> and
<title> constantly (an <a href> in a summary, a <b> in a title), and what a
reader wants from those fields is the words."
  (if (null element)
      nil
      (let ((out (make-string-output-stream)))
        (labels ((walk (e)
                   (dolist (c (element-children e))
                     (if (element-p c) (walk c) (write-string c out)))))
          (walk element))
        (let ((s (get-output-stream-string out)))
          (if trim (string-trim '(#\Space #\Tab #\Newline #\Return) s) s)))))
