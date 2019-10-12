;;; Syntax highlighting server IPC
;;
;; SPDX-License-Identifier: GPL-3.0-or-later
(defpackage #:blog-generator.syntax-hl
  (:use #:cl #:iterate #:blog-generator.utils)
  (:local-nicknames
   (#:alx #:alexandria)
   (#:reader #:blog-generator.reader))
  (:export
   #:*pygments-server-path*
   #:change-language
   #:highlight-code
   #:highlighting-error
   #:highlighting-error-code
   #:highlighting-error-data
   #:restart-server-and-retry
   #:with-highlighting-server))
(in-package #:blog-generator.syntax-hl)

(declaim (optimize (speed 2) (safety 3) (debug 2) (space 1) (compilation-speed 0)))

(define-condition* highlighting-error (error)
  ((error-code :initform :other :type keyword)
   (data :initform nil))
  :documentation "Attempting to communicate with the syntax highlighting server
resulted in an error.
ERROR-CODE indicates what happened, DATA includes additional information."
  :report "Syntax highlighting communication error ~S, data~% ~S")

(defvar *pygments-server-process* nil
  "The `uiop:process-info' representing the currently running syntax
highlighting server, or nil if none.")

(-type *pygments-server-guarded-p* boolean)
(defvar *pygments-server-guarded-p* nil
  "A boolean indicating if there's an active `%with-highlighting-server'
on the call stack.")

(-type *pygments-server-path* (or string pathname))
(defvar *pygments-server-path*
  (uiop:merge-pathnames* "pygments_server.py" *default-pathname-defaults*)
  "The path the to syntax highlighting server executable.

The initial value is just a hopeful guess, it may need changing.")

(-> launch-highlighting-server () t)
(defun launch-highlighting-server ()
  "Launch the syntax highlighting server, returning the
`uiop:process-info' representing the started server."
  (uiop:launch-program (list *pygments-server-path*)
                       :output :stream
                       :error-output :stream
                       :input :stream))

(defun cleanup-highlighting-server (process)
  "Cleanup the remainders of the syntax highlighting server process.
If the server is still running, this function will wait for it to end."
  (uiop:close-streams process)
  (uiop:wait-process process)
  (setf *pygments-server-process* nil))

(-> run-highlighting-command (string &rest string) (values))
(defun run-highlighting-command (command &rest arguments)
  "Run a syntax highlighting COMMAND with a list of ARGUMENTS, waiting
for completion.
All of ARGUMENTS should be strings and shouldn't contain new line
characters.

If an error occurs serverside, a lisp error is signaled."
  (let ((process
         (or *pygments-server-process* (highlighting-error :server-not-running nil))))
    (let ((input-stream (uiop:process-info-input process)))
      (format input-stream "~A~%~{~A~%~}" command arguments)
      (finish-output input-stream))
    (let ((output-stream (uiop:process-info-output process)))
      (trivia:match (read-line output-stream nil)
        ((equal ":done")
         ;; Nothing to do, the server returned success.
         nil)
        ((null)
         ;; The output stream was closed? Read the error stream to see what
         ;; went wrong.
         (let* ((error-stream (uiop:process-info-error-output process))
                (error-message (uiop:slurp-stream-string error-stream)))
           (cleanup-highlighting-server process)
           (highlighting-error :server-error error-message)))
        (response
         ;; Received an unexpected response, kill the server and signal.
         (uiop:terminate-process process)
         (cleanup-highlighting-server process)
         (highlighting-error :unexpected-response response))))))

(-> stop-highlighting-server () (values))
(defun stop-highlighting-server ()
  (alx:when-let ((process *pygments-server-process*))
    (flet ((terminate-process-if-still-active (e)
             (declare (ignore e))
             (ignore-errors (uiop:terminate-process process))))
      (handler-bind ((error #'terminate-process-if-still-active))
        (run-highlighting-command ":quit")))
    (cleanup-highlighting-server process)))

(-> restart-highlighting-server () t)
(defun relaunch-highlighting-server-if-needed ()
  "Restart the highlighting server if it has died.

There must be a guarding `%with-highlighting-server' active on the call
stack, i.e. `*pygments-server-guarded-p*' must be non-nil."
  (unless *pygments-server-process*
    (assert *pygments-server-guarded-p* nil
            "Cannot restart the Pygments server without an active guard")
    (setf *pygments-server-process* (launch-highlighting-server))))

(-> %with-highlighting-server ((-> () t)) t)
(defun %with-highlighting-server (thunk)
  (cond
    (*pygments-server-process*
     ;; The server is active, so just use it.
     (funcall thunk))
    (*pygments-server-guarded-p*
     ;; The server died, but there is a guarding `%with-highlighting-server' on
     ;; the stack, relaunch the server and let the guard worry about cleanup.
     (relaunch-highlighting-server-if-needed)
     (funcall thunk))
    (t
     ;; There's no active server and there's no guard, so launch the server and
     ;; take care of it.
     (let ((*pygments-server-process* (launch-highlighting-server))
           (*pygments-server-guarded-p* t))
       (unwind-protect (funcall thunk)
         (stop-highlighting-server))))))

(defmacro with-highlighting-server (&body body)
  "Run the BODY forms with the syntax highlighting server started.

If the syntax highlighting server is already running, this macro behaves
just like `progn', otherwise it wraps the BODY in an `unwind-protect' to
ensure the syntax highlighting server is closed whether the control
leaves normally or abnormally."
  (alx:with-gensyms (thunk)
    `(flet ((,thunk () ,@body))
       (declare (dynamic-extent (function ,thunk)))
       (%with-highlighting-server #',thunk))))

(-> %highlight-code (string string) list)
(defun %highlight-code (code language)
  "Syntax highlight CODE in LANGUAGE, returning HTSL forms."
  (values
   (with-highlighting-server
     (uiop:with-temporary-file (:pathname result-path)
       (uiop:with-temporary-file (:stream code-stream :pathname code-path :direction :output)
         (write-string code code-stream)
         (finish-output code-stream)
         (run-highlighting-command
          ":highlight" language (namestring code-path) (namestring result-path)))
       (uiop:with-input-file (result-stream result-path)
         (first (reader:read-forms result-stream)))))))

(-> highlight-code (string string) list)
(defun highlight-code (code language)
  "Syntax highlight CODE in LANGUAGE, returning HTSL forms.
This is the high-level entry point that offers user-friendly restarts."
  (let ((actual-language language))
    (iter
      (restart-case (return (%highlight-code code actual-language))
        (change-language (new-language)
          :report "Change the Pygments language name."
          :interactive (lambda () (format t "~&Language to use: ") (list (read-line)))
          (setf actual-language new-language))
        (restart-server-and-retry ()
          :report "Restart the Pygments server if needed and retry evaluation."
          (relaunch-highlighting-server-if-needed))))))
