;;; Sexp representation of HTML
;;
;; SPDX-License-Identifier: GPL-3.0-or-later
(defpackage #:blog-generator.htsl
  (:use #:cl #:iterate #:blog-generator.utils)
  (:local-nicknames
   (#:alx #:alexandria)
   (#:string-table #:blog-generator.string-table))
  (:export
   #:convert-document
   #:define-tag
   #:define-tag-macro
   #:get-tag-by-name
   #:htsl-error
   #:invalid-sexp
   #:invalid-sexp-sexp
   #:invalid-tag-head
   #:invalid-tag-head-sexp
   #:make-tag
   #:nesting-error
   #:nesting-error-actual-context
   #:nesting-error-allowed-contexts
   #:nesting-error-tag
   #:tag
   #:tag-allowed-contexts
   #:tag-attributes
   #:tag-child-context
   #:tag-expander
   #:tag-name
   #:tag-omit-closing-tag
   #:unknown-attribute
   #:unknown-attribute-attribute
   #:unknown-attribute-tag
   #:unknown-tag
   #:unknown-tag-tag-name
   #:wrong-attribute-type
   #:wrong-attribute-type-attribute
   #:wrong-attribute-type-expected-type
   #:wrong-attribute-type-tag
   #:wrong-attribute-type-value))
(in-package #:blog-generator.htsl)

(declaim (optimize (speed 2) (safety 3) (debug 2) (space 1) (compilation-speed 0)))


;;; Tag definition machinery

(eval-and-compile
  (defstruct (tag (:copier nil) (:predicate nil))
    "Represents a known HTML tag or a tag macro.

Slots:
 - NAME: the normalized tag name, a string.
 - EXPANDER: a function (ATTRIBUTES &rest BODY) that performs the
   macro-expansion of a tag macro, in which case the following slots are
   ignored because they don't make sense for tag macros, or nil, in
   which case it's a regular tag.
 - ALLOWED-CONTEXTS: may be one of:
    - T, which allows the tag to be used anywhere,
    - a keyword, which allows the tag to be used only in that one
      particular context,
    - a list of keywords, which allows the tag to be used in any of the
      listed contexts,
    - an arbitrary function taking (CURRENT-CONTEXT) and returning
      non-nil iff the tag is allowed there.
 - CHILD-CONTEXT: may be one of:
    - nil, which prohibits any children whatsoever,
    - a keyword, which allows children that are allowed in that context,
    - an arbitrary function taking (CURRENT-CONTEXT) and returning a
      keyword representing the child context or nil.
 - OMIT-CLOSING-TAG: a boolean determining whether the closing tag will
   be omitted in the generated HTML.
 - ATTRIBUTES: a list of attributes allowed for the tag in addition to
   the global attributes. The elements are of the form (NAME . TYPE),
   where NAME is a keyword representing the name of the attribute, and
   TYPE is either `string', `integer' or `boolean'."
    (name
     nil
     :read-only t
     :type string)
    (expander
     nil
     :read-only t
     :type (nullable (-> (t &rest t) t)))
    (allowed-contexts
     nil
     :read-only t
     :type (or
            (eql t)
            keyword
            list
            (-> (keyword) t)))
    (child-context
     nil
     :read-only t
     :type (or
            null
            keyword
            (-> (keyword) (nullable keyword))))
    (omit-closing-tag
     nil
     :read-only t
     :type boolean)
    (attributes
     '()
     :read-only t
     :type list)))

(-type *tag-table* string-table:string-table)
(defvar *tag-table* (string-table:make-string-table)
  "String table mapping tag names (i.e. strings coming from downcasing the
symbol used as the name in `define-tag') to their corresponding tag
structs.")

(-> encode-symbol (symbol) string)
(eval-and-compile
  (defun encode-symbol (symbol)
    "Encode the SYMBOL for a tag or attribute name."
    (string-downcase (symbol-name symbol))))

(-> encode-tag-name ((or symbol string)) string)
(eval-and-compile
  (defun encode-tag-name (tag-name)
    (trivia:ematch tag-name
      ((type symbol) (encode-symbol tag-name))
      ((type string) tag-name))))

(-> get-tag-by-name ((or symbol string)) (nullable tag))
(defun get-tag-by-name (tag-name)
  (string-table:get *tag-table* (encode-tag-name tag-name)))

(-> %define-tag (tag) tag)
(defun %define-tag (tag)
  (setf (string-table:get *tag-table* (tag-name tag)) tag))

(defmacro define-tag
    (tag-name
     &key
       allowed-contexts
       (child-context nil child-context-specified)
       omit-closing-tag
       attributes)
  (assert allowed-contexts nil "Refusing to define a tag with no allowed contexts")
  (assert child-context-specified nil "Refusing to define a tag with no specified child context")
  `(progn
     (%define-tag
      (make-tag
       :name ,(encode-tag-name tag-name)
       :allowed-contexts ,allowed-contexts
       :child-context ,child-context
       :omit-closing-tag ,omit-closing-tag
       :attributes ,attributes))
     nil))

(defmacro define-tag-macro (tag-name lambda-list &body body)
  `(%define-tag
    (make-tag
     :name ,(encode-tag-name tag-name)
     :expander (lambda ,lambda-list ,@body))))


;;; Condition types

(define-condition htsl-error (error)
  ()
  (:documentation "Base type for all errors signaled by the HTSL engine."))

(define-condition* unknown-attribute (htsl-error)
  ((tag :type tag)
   (attribute :type symbol))
  :documentation "An unknown ATTRIBUTE was found.
This can signify a typo in the attribute name or an attempt to use an
attribute for a TAG for which it's nonsensical."
  :report "Unknown attribute ~1@*~S of tag~% ~0@*~S")

(define-condition* wrong-attribute-type (htsl-error)
  ((tag :type tag)
   (attribute :type symbol)
   expected-type
   value)
  :documentation "The VALUE of ATTRIBUTE specified for TAG was of the wrong type.
The VALUE was not of the EXPECTED-TYPE."
  :report
  "Value~% ~3@*~S~%of attribute ~1@*~S of tag~% ~0@*~S~%is not of type ~2@*~S.")

(define-condition* invalid-sexp (htsl-error)
  (sexp)
  :documentation "The SEXP couldn't be parsed by HTSL at all, whether
because it's of a wrong type, like a vector or a float, or because it's
seriously malformed."
  :report "The sexp~% ~S~%couldn't be parsed as HTSL.")

(define-condition* invalid-tag-head (invalid-sexp)
  (sexp)
  :documentation "The SEXP couldn't be parsed because it's not a valid
tag head (i.e. tag name and attribute list)."
  :report "The sexp~% ~S~%coudln't be parsed as a HTSL tag head.")

(define-condition* unknown-tag (htsl-error)
  ((tag-name :type (or symbol string)))
  :documentation "An unknown TAG-NAME was found."
  :report "~S is not a known tag name.")

(define-condition* nesting-error (htsl-error)
  ((tag :type (or tag (eql :raw-text)))
   allowed-contexts
   (actual-context :type keyword))
  :documentation "A nesting error occurred: TAG was found as a in
context type ACTUAL-CONTEXT, but it is allowed only in contexts
ALLOWED-CONTEXTS."
  :report "Tag~% ~0@*~S~%occured in context ~2@*~S, but is allowed only in ~1@*~S.")


;;; Conversion machinery

(-type *output-stream* stream)
(defvar *output-stream*)
(setf (documentation '*output-stream* 'variable)
      "The output stream used during conversion process, dynamically bound
purely for convenience, to avoid passing it around everywhere.")

(-> escape-string (string t) string)
(defun escape-string (string in-attribute)
  "Escape STRING by entity-encoding problematic characters.
With IN-ATTRIBUTE non-nil, encode the string for use in HTML attribute
values, otherwise encode it for use as regular text node."
  (labels
      ((escape-in-text (character)
         (trivia:match character
           (#\< "&lt;")
           (#\> "&gt;")
           (#\& "&amp;")))
       (escape-in-attribute (character)
         (trivia:match character
           (#\" "&quot;")
           (_ (escape-in-text character))))
       (impl (string escape-func)
         (alx:if-let ((first-to-escape (position-if escape-func string)))
           ;; Slow path for strings that need escaping: loop over the string,
           ;; looking the characters that need escaping, writing whole
           ;; substrings from the current index to the next character to escape.
           ;; Since characters to escape aren't very common, this saves a lot of
           ;; time over a naïve loop writing one character at a time.
           (with-output-to-string (stream)
             (let ((index 0)
                   (end-index (length string))
                   (to-escape-index first-to-escape))
               (iter
                 (write-string string stream :start index :end to-escape-index)
                 (write-string (funcall escape-func (aref string to-escape-index)) stream)
                 (setf index (1+ to-escape-index))
                 (while (and (< index end-index)
                             (setf to-escape-index (position-if escape-func string :start index))))
                 (finally (when (< index end-index)
                            (write-string string stream :start index))))))
           ;; Fast path for strings that don't contain any characters that need
           ;; escaping: just return the string itself.
           string))
       (dispatch (string)
         ;; Do not fold the if into (impl string (if …)), doing so prevents
         ;; escape-in-{attribute,text} inlining in SBCL and inlining them is a
         ;; huge gain; without it may as well not bother specializing in the
         ;; first place.
         (if in-attribute
             (impl string #'escape-in-attribute)
             (impl string #'escape-in-text))))
    (declare (ftype (-> (character) (nullable string)) escape-in-text escape-in-attribute))
    (declare (inline escape-in-text escape-in-attribute impl dispatch))
    (trivia:match string
      ;; Specialize for the most common string types, because this function is
      ;; the hottest part of HTSL -> HTML conversion process.
      ((type (simple-array base-char (*))) (dispatch string))
      ((type (simple-array character (*))) (dispatch string))
      (_ (dispatch string)))))

(alx:define-constant +global-attributes+
    '((:class . string)
      (:id . string)
      (:lang . string))
  :test #'equal
  :documentation "List of tag attributes that are allowed on all elements and
their corresponding value types.
The types follow the same semantics as `tag-attributes'.")

(deftype encoded-attribute ()
  '(cons string (or string (eql t))))

(-> encode-attribute-by-type (tag keyword t symbol) (nullable encoded-attribute))
(defun encode-attribute-by-type (tag name value type)
  "Encode a single attribute according to its TYPE.
Return a value suitable as an element of the list returned by
`encode-attributes'.

TAG is used only for error reporting.

If the attribute is a boolean attribute that's nil, a nil will be
returned, which is contrary to `encode-attributes', which omits the
attribute entirely."
  (macrolet
      ((check-attribute-type (type-symbol)
         `(unless (typep value ',type-symbol)
            (wrong-attribute-type tag name ',type-symbol value))))
    (let ((encoded-name (encode-symbol name)))
      (trivia:ematch type
        ('string
         (check-attribute-type string)
         (cons encoded-name (escape-string value t)))
        ('integer
         (check-attribute-type integer)
         (cons encoded-name (write-to-string value)))
        ('boolean
         (check-attribute-type boolean)
         (and value (cons encoded-name t)))))))

(-> encode-attributes (tag list) list)
(defun encode-attributes (tag attribute-plist)
  "Parse the ATTRIBUTE-PLIST for TAG, raising an error if the attribute
name is unknown or its value is of the wrong type.

Return an alist of encoded attribute names (see `encode-symbol') and
their corresponding encoded values, which are either T if it's a boolean
attribute or already-escaped strings."
  (let ((tag-attributes (tag-attributes tag)))
    (iter
      (for (name value) on attribute-plist by #'cddr)
      (alx:if-let ((type (or (cdr (assoc name tag-attributes :test #'eq))
                             (cdr (assoc name +global-attributes+ :test #'eq))
                             (when (uiop:string-prefix-p "data-" (encode-symbol name))
                               'string))))
        (collect (encode-attribute-by-type tag name value type))
        (unknown-attribute tag name)))))

(-> serialize-encoded-attribute (encoded-attribute) string)
(defun serialize-encoded-attribute (encoded-attribute)
  "Serialize a single encoded attribute into a string.
See `encode-attributes' for the description of the encoded format."
  (trivia:ematch encoded-attribute
    ((cons name 't)
     ;; If it's a boolean attribute, use the short form, which is just the
     ;; attribute name.
     name)
    ((cons name value)
     ;; SBCL forgets the actual types for some reason, despite them being
     ;; specified in the ftype, so it's unable to optimize this format without
     ;; explicit type annotations.
     (declare (type string name value))
     (format nil "~A=\"~A\"" name value))))

(alx:define-constant +raw-text-contexts+ '(:flow :phrasing :text-only)
  :test #'equal
  :documentation "The list of contexts in which a raw text node is allowed.")

(-> verify-tag-context (tag keyword) t)
(defun verify-tag-context (tag context)
  "Signal an `nesting-error' if TAG is not allowed in CONTEXT."
  (let ((allowed-contexts (tag-allowed-contexts tag)))
    (unless (trivia:ematch allowed-contexts
              ('t t)
              ((type keyword) (eq context allowed-contexts))
              ((type list) (member context allowed-contexts :test #'eq))
              ((type function) (funcall allowed-contexts context)))
      (nesting-error tag allowed-contexts context))))

(-> get-child-context (tag keyword) (nullable keyword))
(defun get-child-context (tag context)
  (let ((child-context (tag-child-context tag)))
    (if (functionp child-context)
        (funcall child-context context)
        child-context)))

(-> convert-known-tag (tag list t keyword) t)
(defun convert-known-tag (tag attributes children context)
  ;; Deal with tag macros before doing anything else, so the rest of this
  ;; function doesn't have to worry about them at all.
  (alx:when-let ((expander (tag-expander tag)))
    (return-from convert-known-tag (convert (apply expander attributes children) context)))
  (verify-tag-context tag context)
  (let ((tag-name (tag-name tag)))
    (let ((encoded-attributes (encode-attributes tag attributes)))
      (format *output-stream* "<~A~{~#[~:; ~A~]~}>"
              (tag-name tag)
              (mapcar #'serialize-encoded-attribute encoded-attributes)))
    (let ((child-context (get-child-context tag context)))
      (iter (for child in children) (convert child child-context)))
    (unless (tag-omit-closing-tag tag)
      (format *output-stream* "</~A>" tag-name))))

(-> convert (t keyword) t)
(defun convert (sexp context)
  "Convert an arbitrary SEXP.
This function checks if the element represented by SEXP is allowed in
the specified CONTEXT."
  (trivia:match sexp
    ;; Is it a tag?
    ((cons tag-name-and-attributes children)
     (multiple-value-bind (tag-name attributes)
         (trivia:match tag-name-and-attributes
           ((cons tag-name attributes) (values tag-name attributes))
           ((type symbol) (values tag-name-and-attributes '()))
           (_ (invalid-tag-head tag-name-and-attributes)))
       (alx:if-let ((tag (get-tag-by-name tag-name)))
         (convert-known-tag tag attributes children context)
         (unknown-tag tag-name))))
    ;; Is it a text node?
    ((type string)
     (unless (member context +raw-text-contexts+ :test #'eq)
       (nesting-error :raw-text +raw-text-contexts+ context))
     (write-string (escape-string sexp nil) *output-stream*))
    (_ (invalid-sexp sexp))))


;;; Public entry point

(-> convert-document (t) string)
(defun convert-document (document-sexp)
  "Convert a complete HTSL document to HTML.
The HTML is returned as a string."
  (with-output-to-string (stream)
    (let ((*output-stream* stream))
      (convert document-sexp :root))))
