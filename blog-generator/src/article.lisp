;;; Article file processing
;;
;; SPDX-License-Identifier: GPL-3.0-or-later
(defpackage #:blog-generator.article
  (:use #:cl #:iterate #:blog-generator.utils)
  (:local-nicknames
   (#:alx #:alexandria)
   (#:reader #:blog-generator.reader)
   (#:reader.article #:blog-generator.reader.article))
  (:export
   #:*section-heading-hook*
   #:*table-of-contents-hook*
   #:article
   #:article-date
   #:article-description
   #:article-root-section
   #:article-topics
   #:article-title
   #:date
   #:date-day
   #:date-month
   #:date-year
   #:date<
   #:generate-body-htsl
   #:make-date
   #:malformed-article
   #:malformed-article-data
   #:malformed-article-error-code
   #:parse-article-forms
   #:section
   #:section-body
   #:section-element-id
   #:section-header
   #:section-href
   #:section-id))
(in-package #:blog-generator.article)

(declaim (optimize (speed 2) (safety 3) (debug 2) (space 1) (compilation-speed 0)))


;;; Simple date handling, for the publication date of articles

(declaim (inline %make-date))
(defstruct (date (:copier nil) (:predicate nil) (:constructor %make-date (year month day)))
  "A self-explanatory representation of a date."
  (year nil :type unsigned-fixnum :read-only t)
  (month nil :type (integer 1 12) :read-only t)
  (day nil :type (integer 1 31) :read-only t))

(defmacro define-byte-constant-array (name contents)
  #+sbcl(declare (sb-ext:muffle-conditions sb-ext:compiler-note))
  (let ((length (length contents)))
    `(progn
       (-type ,name (simple-array (unsigned-byte 8) ,length))
       (alx:define-constant ,name
           (make-array ,length
                       :element-type '(unsigned-byte 8)
                       :initial-contents ',contents)
         :test #'equalp))))

(define-byte-constant-array +common-year-month-lengths+
    (31 28 31 30 31 30 31 31 30 31 30 31))
(define-byte-constant-array +leap-year-month-lengths+
    (31 29 31 30 31 30 31 31 30 31 30 31))

(-> make-date (unsigned-fixnum unsigned-fixnum unsigned-fixnum) date)
(defun make-date (year month day)
  (unless (<= 1 month 12)
    (error 'type-error :expected-type '(integer 1 12) :datum month))
  (let* ((leap-year-p (and (zerop (mod year 4))
                           (or (not (zerop (mod year 100)))
                               (zerop (mod year 400)))))
         (month-length-vec (if leap-year-p +leap-year-month-lengths+ +common-year-month-lengths+))
         (month-length (aref month-length-vec (1- month))))
    (unless (<= 1 day month-length)
      (error 'type-error :expected-type `(integer 1 ,month-length) :datum day)))
  (%make-date year month day))

(-> date< (date date) boolean)
(defun date< (x y)
  "Return T if the date X lies before Y, NIL otherwise."
  (macrolet
      ((generate-comparisons (&rest readers)
         (trivia:match readers
           ((list* reader rest)
            (alx:once-only ((field-of-x `(,reader x))
                            (field-of-y `(,reader y)))
              `(or (< ,field-of-x ,field-of-y)
                   (and (= ,field-of-x ,field-of-y)
                        (generate-comparisons ,@rest))))))))
    (generate-comparisons date-year date-month date-day)))


;;; Article preprocessing

(define-condition* malformed-article (error)
  ((error-code :initform :other :type keyword)
   (data :initform nil))
  :documentation "Couldn't process an article due to an error.
ERROR-CODE indicates what happened, DATA includes additional information."
  :report "Malformed article due to ~S, data~% ~S")

(alx:define-constant +root-section-id+ 'root-section
  :test #'eq
  :documentation "The symbolic ID used for the root section only.")

(defstruct (section (:copier nil) (:predicate nil))
  "A section of an article.

Slots:
 - ID: the article-unique identifier of this section, a symbol.
 - HEADER: the title of this section, a string.
 - CHILDREN: a list of IDs of child sections.
 - BODY: a list of HTSL forms that form the body of this section."
  (id nil :read-only t :type symbol)
  (header nil :read-only t :type string)
  (children '() :read-only t :type list)
  (body nil :read-only t :type list))

(defstruct (article (:copier nil) (:predicate nil))
  "A parsed blog article.

Slots:
 - TITLE: the title of this article, a string.
 - DESCRIPTION: a *short* description of this article, a string.
 - DATE: the publication date of this article, a `date' structure.
 - INHIBIT-TABLE-OF-CONTENTS: if non-nil, the generated document will
   not contain a table of contents.
 - TOPICS: a list of strings indicating the article topics.
 - ROOT-SECTION: a `section' structure representing the root section of
   this article, can be used as the excerpt in the blog archive.
 - SECTIONS-BY-ID: a hash table mapping symbolic section IDs to the
   corresponding `section' structures."
  (title nil :read-only t :type string)
  (description nil :read-only t :type string)
  (date nil :read-only t :type date)
  (inhibit-table-of-contents nil :read-only t :type boolean)
  (topics nil :read-only t :type list)
  (root-section nil :read-only t :type section)
  (sections-by-id (make-hash-table :test #'eq) :read-only t :type hash-table))

(-> extract-plist-prefix (list) (values list list))
(defun extract-plist-prefix (list)
  "Given a LIST, return its plist prefix and the remainder.
The plist prefix is the longest even-length prefix in which every even
element, counting from 0, is a keyword."
  (let ((remainder list))
    (iter (until (atom remainder))
          (for (key value . next-remainder) = remainder)
          (while (keywordp key))
          (nconcing (list key value) into plist)
          (setf remainder next-remainder)
          (finally (return (values plist remainder))))))

(-> parse-defarticle (t) article)
(defun parse-defarticle (defarticle-form)
  "Parse the given DEFARTICLE-FORM, returning an article structure.

The returned article has empty sections-by-id, because this function
doesn't know about any sections other than the root one; that slot needs
to be filled later."
  (trivia:match defarticle-form
    ((cons 'reader.article:defarticle form-body)
     (multiple-value-bind (article-metadata root-section-body) (extract-plist-prefix form-body)
       (destructuring-bind (&key title description date children inhibit-table-of-contents topics)
           article-metadata
         (alx:when-let ((first-non-string (car (member-if-not #'stringp topics))))
           (malformed-article :non-string-topic first-non-string))
         (make-article
          :title title
          :description description
          :date (apply #'make-date date)
          :topics topics
          :inhibit-table-of-contents inhibit-table-of-contents
          :root-section (make-section :id +root-section-id+
                                      :header ""
                                      :children children
                                      :body root-section-body)))))
    (_
     (malformed-article :not-defarticle defarticle-form))))

(-> parse-defsection (t) section)
(defun parse-defsection (defsection-form)
  "Parse the given DEFSECTION-FORM, return a section structure."
  (trivia:match defsection-form
    ((list* 'reader.article:defsection id form-body)
     (multiple-value-bind (section-metadata section-body) (extract-plist-prefix form-body)
       (destructuring-bind (&key header children) section-metadata
         (make-section
          :id id
          :header header
          :children children
          :body section-body))))
    (_
     (malformed-article :not-defsection defsection-form))))

(-> verify-section-graph (hash-table list) (values))
(defun verify-section-graph (sections-by-id sections)
  "Verify that the section graph represented by the list SECTIONS is
valid.

SECTIONS-BY-ID is an *empty* hash table that will be populated to map
symbolic section IDs to the corresponding section objects.

A valid section graph meets the following invariants:
 - there are no duplicate section IDs,
 - all sections are reachable from the root section,
 - each section is referenced only once,
 - the root section is not referenced by any other section."
  (assert (zerop (hash-table-count sections-by-id)))
  (let ((section-references (make-hash-table :test #'eq)))
    (iter (for section in sections)
          (let ((id (section-id section)))
            (when (gethash id sections-by-id)
              (malformed-article :duplicate-section-id id))
            (setf (gethash id sections-by-id) section)
            (setf (gethash id section-references) '())))
    ;; Use depth-first search because it's the easiest way to verify the
    ;; invariants.
    (let ((seen-sections (make-hash-table :test #'eq))
          (stack (list (cons nil +root-section-id+))))
      (iter (while stack)
            (trivia:let-match1 (cons parent-id section-id) (pop stack)
              (when parent-id
                (push parent-id (gethash section-id section-references)))
              (unless (gethash section-id seen-sections)
                (setf (gethash section-id seen-sections) t)
                (iter (for child-id in (section-children (gethash section-id sections-by-id)))
                      (unless (gethash child-id sections-by-id)
                        (malformed-article :undefined-section-id child-id))
                      (push (cons section-id child-id) stack))))))
    ;; Now the information has been extracted from the graph, see if
    ;; there are any violations and report them as necessary.
    (iter (for section in sections)
          (let ((id (section-id section)))
            (if (eq id +root-section-id+)
                (alx:when-let ((references (gethash id section-references)))
                  (malformed-article :root-section-referenced references))
                (trivia:match (gethash id section-references)
                  ((list _)
                   nil)
                  ((null)
                   (malformed-article :unreachable-section id))
                  (references
                   (malformed-article :section-used-multiple-times (cons id references))))))))
  (values))

(-> parse-article-forms (list) article)
(defun parse-article-forms (article-forms)
  "Parse the list of ARTICLE-FORMS, returning a complete `article'
structure as the first value and the hash table mapping the symbolic section IDs"
  (let* ((article (parse-defarticle (first article-forms)))
         (root-section (article-root-section article))
         (sections (mapcar #'parse-defsection (rest article-forms)))
         (all-sections (cons root-section sections)))
    (verify-section-graph (article-sections-by-id article) all-sections)
    article))

(-> resolve-section-nesting (article) list)
(defun resolve-section-nesting (article)
  "Resolve the section nesting in the section graph of ARTICLE.

Return a nested list where each element is of the form (SECTION .
CHILDREN), where SECTION is a section object and CHILDREN is a nested
list of the same format."
  (let ((sections-by-id (article-sections-by-id article)))
    (labels
        ((impl (section)
           (cons section
                 (iter (for child-id in (section-children section))
                       (collect (impl (gethash child-id sections-by-id)))))))
      (declare (ftype (-> (section) list) impl))
      (impl (article-root-section article)))))

(-> section-element-id (section) string)
(defun section-element-id (section)
  "Convert the ID of SECTION to a string suitable for HTSL :id
attribute."
  (string-downcase (symbol-name (section-id section))))

(-> section-href (section) string)
(defun section-href (section)
  "Convert the ID of SECTION to a string suitable for HTSL :href
attribute of an A element."
  (format nil "#~A" (section-element-id section)))

(-> generate-table-of-contents (list) list)
(defun generate-table-of-contents (nested-sections)
  "Generate a nested ordered HTSL list representing a table of contents.
NESTED-SECTIONS is the children of the root section, in the format
returned by `resolve-section-nesting'."
  (labels
      ((section-link (section) `((a :href ,(section-href section)) ,(section-header section)))
       (impl (children)
         (when children
           `((ol
              ,@(iter (for (section . next-children) in children)
                      (collect `(li ,(section-link section)
                                    ,@(impl next-children)))))))))
    (declare (ftype (-> (section) list) section-link))
    (declare (ftype (-> (list) list) impl))
    (first (impl nested-sections))))

;; These hooks are public customization points.
(-type *table-of-contents-hook* (-> (list) list))
(defvar *table-of-contents-hook* #'identity
  "Hook enabling the user to transform the generated table of contents.

It is called with one argument, the table of contents as an HTSL form.
The returned value is used directly as a HTSL form.

The default is just `cl:identity', returning the table of contents
unchanged.")

(-type *section-heading-hook* (-> (section list) list))
(defvar *section-heading-hook*
  (lambda (section form)
    (declare (ignore section))
    form)
  "Hook enabling the user to transform the generated heading of each
section.

It is called with two arguments (SECTION FORM): SECTION being the
section structure this heading pertains to, FORM being the HTSL form of
the heading.
The returned value is used directly as a HTSL form.

The default ignores SECTION and returns FORM unchanged.")

(-> generate-body-htsl (article) list)
(defun generate-body-htsl (article)
  "Convert the ARTICLE to a list of HTSL elements, suitable for being
the children of the article HTSL tag."
  (labels
      ((make-regular-section-header (nesting-level section)
         (funcall *section-heading-hook*
                  section
                  `(,(intern (format nil "H~A" (min 6 nesting-level)) reader:+article-package+)
                     ,(section-header section))))
       (impl (nesting-level section-and-children)
         (trivia:let-match1 (cons section children) section-and-children
           (cond
             ((= 1 nesting-level)
              ;; The root section is special in that it doesn't have a section
              ;; tag, an ID or a header, is immediately followed by the table of
              ;; contents, and doesn't contain its children in HTSL.
              `(,@(section-body section)
                  ,@(when (and children (not (article-inhibit-table-of-contents article)))
                      (list
                       (funcall *table-of-contents-hook*
                                (generate-table-of-contents children))))
                  ,@(iter (for child in children)
                          (collect (impl (1+ nesting-level) child)))))
             (t
              ;; Non-root sections are regular.
              `((section :id ,(section-element-id section))
                ,(make-regular-section-header nesting-level section)
                ,@(section-body section)
                ,@(iter (for child in children)
                        (collect (impl (1+ nesting-level) child)))))))))
    (declare (ftype (-> (fixnum section) list) make-regular-section-header))
    (declare (ftype (-> (fixnum (cons section list)) list) impl))
    (impl 1 (resolve-section-nesting article))))
