;;; Various utilities
;;
;; SPDX-License-Identifier: GPL-3.0-or-later
(defpackage #:blog-generator.utils
  (:use #:cl #:iterate)
  (:local-nicknames
   (#:alx #:alexandria))
  (:export
   #:->
   #:-type
   #:define-condition*
   #:define-immutable-structure
   #:eval-and-compile
   #:nullable
   #:unsigned-fixnum))
(in-package #:blog-generator.utils)

(declaim (optimize (speed 2) (safety 3) (debug 2) (space 1) (compilation-speed 0)))



(deftype unsigned-fixnum ()
  "Convenience alias for (and fixnum unsigned-byte)."
  '(and fixnum unsigned-byte))

(deftype nullable (type)
  "Convenience alias for (or null TYPE)."
  `(or null ,type))

(deftype -> (lambda-list values)
  "Convenience alias for (function LAMBDA-LIST VALUES)."
  `(function ,lambda-list ,values))

(defmacro -> (function lambda-list values)
  "Declaim that FUNCTION is of the type (function LAMBDA-LIST VALUES).
FUNCTION may be a list, in which case all functions named in that list are
declaimed to be of that type."
  `(declaim (ftype (-> ,lambda-list ,values) ,@(alx:ensure-list function))))

(defmacro -type (variable type)
  "Declaim that VARIABLE is of the TYPE.
VARIABLE may be a list, in which case all variables named in that list are
declaimed to be of that type."
  `(declaim (type ,type ,@(alx:ensure-list variable))))

(defmacro eval-and-compile (&body body)
  "Convenience shorthand for `(eval-when (:compile-toplevel
:load-toplevel :execute) BODY)'.

Name from Emacs Lisp."
  `(eval-when (:compile-toplevel :load-toplevel :execute)
     ,@body))

(defmacro define-condition*
    (name (&rest parents) (&rest slots) &key documentation report omit-error-function)
  "Define a condition type using `define-condition' and a boa function
with the same name as the condition itself that signals the condition
using `error'.

NAME and PARENTS are passed directly to `define-condition'.
SLOTS is a list of slot specs as in `define-condition' with some useful
defaults:
 - If `:initarg' is not present, a keyword with the same name as the
   slot itself will be used.
 - If `:initform' is not present, an `error' form will be used to
   signify the argument is required.
 - If `:reader' is not present, a symbol whose name is the concatenation
   of the condition NAME, a dash, and the slot name, in NAME's package,
   will be used.

DOCUMENTATION is *required*, macro expansion will signal an error if
it's nil.

REPORT is *required*, macro expansion will signal an error if it's nil.
It shall either be a format control string for a `format' call that is
passed all slots in their definition order, or a two-argument function,
either a name or a lambda, just like in `define-condition'.

With non-nil OMIT-ERROR-FUNCTION, the boa signaling function will be
omitted."
  ;; Dear SBCL, please don't report missed optimization in the *macro
  ;; body itself*.
  #+sbcl(declare (sb-ext:muffle-conditions sb-ext:compiler-note))
  (unless documentation
    (error "Refusing to define a condition type ~S with no documentation" name))
  (unless report
    (error "Refusing to define a condition type ~S with no report formatting" name))
  (let ((package (symbol-package name)))
    (flet
        ((cook-slot (slot-spec)
           (let ((unspecified-tag '#:unspecified))
             (macrolet
                 ((use-if-specified-else-default (form default)
                    (alx:once-only (form)
                      `(if (eq ,form unspecified-tag) ,default ,form))))
               (flet
                   ((make-initarg (slot-name form)
                      (use-if-specified-else-default
                       form (alx:make-keyword slot-name)))
                    (make-initform (slot-name form)
                      (use-if-specified-else-default
                       form `(error ,(format nil "Condition slot ~S is required" slot-name))))
                    (make-reader (slot-name form)
                      (use-if-specified-else-default
                       form (intern (format nil "~S-~S" name slot-name) package))))
                 (trivia:ematch slot-spec
                   ((type symbol)
                    `(,slot-spec
                      :initarg ,(make-initarg slot-spec unspecified-tag)
                      :initform ,(make-initform slot-spec unspecified-tag)
                      :reader ,(make-reader slot-spec unspecified-tag)
                      :type t))
                   ((list* slot-name options)
                    `(,slot-name
                      :initarg ,(make-initarg slot-name (getf options :initarg unspecified-tag))
                      :initform ,(make-initform slot-name (getf options :initform unspecified-tag))
                      :reader ,(make-reader slot-name (getf options :reader unspecified-tag))
                      :type ,(getf options :type t)))))))))
      (declare (ftype (-> ((or symbol list)) list) cook-slot))
      (let ((cooked-slots (mapcar #'cook-slot slots)))
        `(progn
           (define-condition ,name ,parents
             ,cooked-slots
             (:documentation ,documentation)
             (:report
              ,(if (stringp report)
                   (alx:with-gensyms (condition stream)
                     `(lambda (,condition ,stream)
                        ;; Condition printers are mostly used for debugging, so
                        ;; optimize them for space rather than speed.
                        (declare (optimize (speed 1) (space 3)))
                        (format ,stream ,report
                                ,@(iter (for slot in cooked-slots)
                                        (collect `(,(getf (rest slot) :reader) ,condition))))))
                   report)))
           ,@(unless omit-error-function
               `((-> ,name
                     ,(iter (for slot in cooked-slots)
                            (collect (getf (rest slot) :type)))
                     nil)
                 (defun ,name
                     ,(iter (for slot in cooked-slots) (collect (first slot)))
                   ;; Raising errors doesn't need to be fast, so optimize accordingly.
                   (declare (optimize (speed 1) (space 3) (safety 3) (debug 3)))
                   (error ',name
                          ,@(iter (for (slot-name . properties) in cooked-slots)
                                  (nconcing `(,(getf properties :initarg) ,slot-name))))))))))))

(defmacro define-immutable-structure (type-name (&rest constructors) &body slots)
  "Define TYPE-NAME as a structure whose SLOTS are all read-only.

Each slot should be of the form (SLOT-NAME-AND-TYPE) or
\(SLOT-NAME-AND-TYPE INIT-FORM), where SLOT-NAME-AND-TYPE is either a list
\(SLOT-NAME TYPE), or a bare symbol SLOT-NAME with TYPE assumed to be T.
If INIT-FORM is not specified, it defaults to
`(alexandria:required-argument 'SLOT-NAME)'.

If the first slot is a string, it will be used as the documentation string
of the type.

The structure will have no type predicate or copier.

CONSTRUCTORS is a list of constructors, where each constructor is passed
to `defstruct' as `(:constructor …)' with no other changes.

Additionally, a boa destructuring pattern for Trivia is defined under the
name TYPE-NAME.

While it is technically possible to inherit from a type defined with
`define-immutable-structure', it is a bad idea, because on implementations
that support it the type will be declared as frozen (sealed)."
  (multiple-value-bind (documentation slots)
      (trivia:match slots
        ((list* (and (type string) documentation) rest)
         (values documentation rest))
        (_
         (values nil slots)))
    (let ((cooked-slots
           (labels
               ((get-name-and-type (name-and-type)
                  (trivia:match name-and-type
                    ((symbol) (values name-and-type t))
                    ((list name type) (values name type))
                    (_ (error "This is not a valid slot name and type: ~S" name-and-type))))
                (cook-slot (slot)
                  (trivia:match slot
                    ((list slot-name-and-type)
                     (multiple-value-bind (name type) (get-name-and-type slot-name-and-type)
                       `(,name (alx:required-argument ',name) :read-only t :type ,type)))
                    ((list slot-name-and-type initform)
                     (multiple-value-bind (name type) (get-name-and-type slot-name-and-type)
                       `(,name ,initform :read-only t :type ,type)))
                    (_
                     (error "This is not a valid slot: ~S" slot)))))
             (mapcar #'cook-slot slots))))
      `(progn
         (defstruct (,type-name
                      (:copier nil)
                      (:predicate nil)
                      ,@(iter (for constructor in constructors)
                              (collect `(:constructor ,@constructor))))
           ,@(alx:ensure-list documentation)
           ,@cooked-slots)
         #+sbcl(declaim (sb-ext:freeze-type ,type-name))
         (trivia:defpattern ,type-name
             ,(if cooked-slots
                  `(&optional ,@(mapcar #'first cooked-slots))
                  '())
           (list 'and
                 '(type ,type-name)
                 ,@(let ((package (symbol-package type-name)))
                     (iter (for slot in cooked-slots)
                           (let ((slot-name (first slot)))
                             (collect
                                 `(list
                                   'trivia:access
                                   '#',(intern (format nil "~S-~S" type-name slot-name) package)
                                   ,(first slot))))))))
         ',type-name))))
