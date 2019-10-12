;;; Safe restricted reader for article files
;;
;; SPDX-License-Identifier: GPL-3.0-or-later
(defpackage #:blog-generator.reader
  (:use #:cl #:iterate #:blog-generator.utils)
  (:local-nicknames
   (#:alx #:alexandria))
  (:shadow #:reader-error)
  (:export
   #:+article-package+
   #:article-package
   #:read-forms
   #:reader-error
   #:reader-error-message))
(in-package #:blog-generator.reader)

(declaim (optimize (speed 2) (safety 3) (debug 2) (space 1) (compilation-speed 0)))



(define-condition* reader-error (cl:reader-error)
  ((message :initform "" :type string))
  :documentation "The article reader found invalid syntax."
  :report "~A"
  :omit-error-function t)

(-> read-hash-sign (stream character) nil)
(defun read-hash-sign (stream character)
  (declare (ignore character))
  (error 'reader-error
         :message "Forms starting with # are disabled"
         :stream stream))

(-type *readtable-for-articles* readtable)
(defparameter *readtable-for-articles*
  (with-standard-io-syntax
    (let ((readtable (copy-readtable)))
      (set-macro-character #\# #'read-hash-sign nil readtable)
      readtable))
  "The readtable used for reading articles, differs from the standard
readtable in that all forms starting with the # character are
disabled.")

;; There's no :use anything, not even cl, because articles aren't really code.
(defpackage #:blog-generator.reader.article
  ;; For boolean settings.
  (:import-from #:cl #:nil #:t)
  ;; Export the few pseudo-macros the article processor recognizes.
  (:export
   #:defarticle
   #:defsection))

(alx:define-constant +article-package+ (find-package '#:blog-generator.reader.article)
  :test #'eq
  :documentation "The package to read articles in.")

(-> read-forms (stream) list)
(defun read-forms (input-stream)
  "Read the lisp forms of an article from INPUT-STREAM, returning a list
of them."
  (values
   (with-standard-io-syntax
     (let ((*readtable* *readtable-for-articles*)
           (*package* +article-package+)
           (eof-sentinel '#:eof-sentinel))
       (iter (for form = (read input-stream nil eof-sentinel))
             (until (eq eof-sentinel form))
             (collect form))))))
