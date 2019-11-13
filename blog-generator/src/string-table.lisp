;;; String-keyed dictionary
;; Copyright © 2019  Fanael Linithien
;; SPDX-License-Identifier: GPL-3.0-or-later
(defpackage #:blog-generator.string-table
  (:use #:cl #:iterate #:blog-generator.utils)
  (:local-nicknames
   (#:alx #:alexandria))
  (:shadow #:get)
  (:export
   #:get
   #:key-list
   #:make-string-table
   #:string-table))
(in-package #:blog-generator.string-table)

(declaim (optimize (speed 2) (safety 3) (debug 2) (space 1) (compilation-speed 0)))



;; Use a standard hash table with :test #'equal for simplicity and just check if
;; the keys are strings at the public entry points. This provides the desired
;; levels type checking and portability without requiring a custom hash table
;; implementation.
;; Since this is just a thin wrapper over the built-in hash table, mark most
;; functions inline to expose the underlying implementation to optimizing
;; compilers like Python, so they can perform their magic.

(declaim (inline %make-string-table))
(define-immutable-structure string-table ((%make-string-table (ht)))
  "A dictionary data type that only supports strings as the key type."
  (ht :type hash-table))

(declaim (inline make-string-table))
(-> make-string-table () string-table)
(defun make-string-table ()
  "Create an empty string table with the default settings."
  (%make-string-table (make-hash-table :test #'equal)))

(declaim (inline get))
(-> get (string-table string &optional t) (values t t &optional))
(defun get (string-table key &optional default)
  "Return the value corresponding to KEY in STRING-TABLE.
If KEY is not present, return DEFAULT instead.

The secondary return value is non-nil iff KEY is present, to allow
distinguishing values that are present-but-default from not present."
  (gethash key (string-table-ht string-table) default))

(declaim (inline (setf get)))
(-> ((setf get)) (t string-table string &optional t) t)
(defun (setf get) (new-value string-table key &optional default)
  "Set the corresponding value of KEY in STRING-TABLE to NEW-VALUE.

DEFAULT is evaluated, but ignored."
  (declare (ignore default))
  (setf (gethash key (string-table-ht string-table)) new-value))

(declaim (inline key-list))
(-> key-list (string-table) list)
(defun key-list (string-table)
  "Return the list of all keys present in STRING-TABLE."
  (values (alx:hash-table-keys (string-table-ht string-table))))

(defmacro-driver (for key-value-variables in-string-table string-table)
  "Iterate over the keys and value of STRING-TABLE."
  `(,(if generate 'generate 'for)
     ,key-value-variables in-hashtable (string-table-ht ,string-table)))
