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

(-type *pygments-server-path* (or string pathname))
(defvar *pygments-server-path*
  (uiop:merge-pathnames* "pygments_server.py" *default-pathname-defaults*)
  "The path the to syntax highlighting server executable.

The initial value is just a hopeful guess, it may need changing.")

(define-unbound-variable *pygments-server-process*
    "The `uiop:process-info' representing the currently running syntax
highlighting server, or nil if the server has quit.

Bound only for as long as a guarding `with-highlighting-server' is active
on the call stack, so care must be taken to test if it is bound.")

(-> launch-highlighting-server () t)
(defun launch-highlighting-server ()
  "Launch the syntax highlighting server, returning the
`uiop:process-info' representing the started server."
  (uiop:launch-program (list *pygments-server-path*)
                       :output :stream
                       :error-output :output
                       :input :stream))

(defun cleanup-highlighting-server (process)
  "Cleanup the remainders of the syntax highlighting server process.
If the server is still running, this function will wait for it to end."
  (uiop:close-streams process)
  (uiop:wait-process process)
  (setf *pygments-server-process* nil))

(defgeneric send-command (command stream)
  (:documentation "Write COMMAND and its arguments to STREAM in serialized form."))

(defgeneric receive-command-response (command stream)
  (:documentation "Read the serialized form of the response to COMMAND from STREAM."))

(declaim (inline send-simple-string))
(-> send-simple-string (string t) (values))
(defun send-simple-string (string stream)
  "Write STRING as a simple string to STREAM.

STRING should not contain any newline characters, this function doesn't
check this!"
  (write-line string stream)
  (values))

(-> send-multiline-string (string t) (values))
(defun send-multiline-string (string stream)
  "Write STRING as a multiline string to STREAM."
  (let ((index 0)
        (end-index (length string)))
    (iter
      (while (and (< index end-index)
                  (alx:when-let ((new-line-position (position #\Newline string :start index)))
                    (write-char #\> stream)
                    (write-line string stream :start index :end new-line-position)
                    (setf index (1+ new-line-position))
                    t)))
      (finally (when (< index end-index)
                 (write-char #\> stream)
                 (write-line string stream :start index)))))
  (write-line ":done" stream)
  (values))

(-> recover-from-server-error ((nullable string) t) nil)
(defun recover-from-server-error (first-line-of-response stream)
  "Given the FIRST-LINE-OF-RESPONSE, recover from a server error.
STREAM should be the output stream of the server."
  (flet ((kill-server ()
           (let ((process *pygments-server-process*))
             (uiop:terminate-process process)
             (cleanup-highlighting-server process)
             (values))))
    (declare (ftype (-> () (values)) kill-server))
    (trivia:match first-line-of-response
      ((equal ":error")
       ;; This is a regular error, retrieve the error message and continue.
       (highlighting-error :server-error (receive-multiline-response stream)))
      ((null)
       ;; The server closed the pipe for some reason?
       (kill-server)
       (highlighting-error :broken-pipe nil))
      (_
       ;; This is an unexpected response, we don't know what to do with it, so
       ;; just kill the server.
       (kill-server)
       (highlighting-error :unexpected-response first-line-of-response)))))

(-> receive-confirmation (t) (values))
(defun receive-confirmation (stream)
  "Receive the confirmation of command completion from STREAM.

If an error occurs serverside, or an expected response is found, a lisp
error is signaled."
  (trivia:match (read-line stream nil)
    ((equal ":done")
     (values))
    (response
     (recover-from-server-error response stream))))

(-> receive-multiline-response (t) string)
(defun receive-multiline-response (stream)
  "Receive a multiline response from STREAM and return it as a string.

If an error occurs serverside, or an expected response is found, a lisp
error is signaled."
  (with-output-to-string (output-stream)
    (iter (for line = (read-line stream nil))
          (trivia:match line
            ((trivia:string* #\>)
             (write-line line output-stream :start 1))
            ((trivia:guard (not (null)) (string= ":done" line))
             (finish))
            (_
             (recover-from-server-error line stream))))))

(define-immutable-structure quit-command ())

(defmethod send-command ((command quit-command) stream)
  (declare (ignore command))
  (send-simple-string ":quit" stream))

(defmethod receive-command-response ((command quit-command) stream)
  (declare (ignore command))
  (receive-confirmation stream))

(define-immutable-structure highlight-command ((make-highlight-command (language code)))
  ((language string))
  ((code string)))

(defmethod send-command ((command highlight-command) stream)
  (trivia:let-match1 (highlight-command language code) command
    (send-simple-string ":highlight" stream)
    (send-simple-string language stream)
    (send-multiline-string code stream)))

(defmethod receive-command-response ((command highlight-command) stream)
  (declare (ignore command))
  (receive-multiline-response stream))

(-> run-highlighting-command (t) t)
(defun run-highlighting-command (command)
  "Run a syntax highlighting COMMAND with a list of ARGUMENTS, waiting
until completion for its result.

If an error occurs serverside, a lisp error is signaled."
  (let ((process (or (and (boundp '*pygments-server-process*) *pygments-server-process*)
                     (highlighting-error :server-not-running nil))))
    (let ((input-stream (uiop:process-info-input process)))
      (send-command command input-stream)
      (finish-output input-stream))
    (let ((output-stream (uiop:process-info-output process)))
      (receive-command-response command output-stream))))

(-> stop-highlighting-server () (values))
(defun stop-highlighting-server ()
  (alx:when-let ((process *pygments-server-process*))
    (flet ((terminate-process-if-still-active (e)
             (declare (ignore e))
             (ignore-errors (uiop:terminate-process process))))
      (handler-bind ((error #'terminate-process-if-still-active))
        (run-highlighting-command (make-quit-command))))
    (cleanup-highlighting-server process)))

(-> restart-highlighting-server () t)
(defun relaunch-highlighting-server-if-needed ()
  "Restart the highlighting server if it has died.

There must be a guarding `%with-highlighting-server' active on the call
stack, i.e. `*pygments-server-guarded-p*' must be non-nil."
  (unless (boundp '*pygments-server-process*)
    (error "Cannot restart the Pygments server without an active guard"))
  (unless *pygments-server-process*
    (setf *pygments-server-process* (launch-highlighting-server))))

(-> %with-highlighting-server ((-> () t)) t)
(defun %with-highlighting-server (thunk)
  (if (boundp '*pygments-server-process*)
      (if *pygments-server-process*
          ;; The server is active, so just use it.
          (funcall thunk)
          ;; The server died, but there is a guarding `%with-highlighting-server' on
          ;; the stack, relaunch the server and let the guard worry about cleanup.
          (progn
            (relaunch-highlighting-server-if-needed)
            (funcall thunk)))
      ;; There's no active server and there's no guard, so launch the server and
      ;; take care of it.
      (let ((*pygments-server-process* (launch-highlighting-server)))
        (unwind-protect (funcall thunk)
          (stop-highlighting-server)))))

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
     (let ((string (run-highlighting-command (make-highlight-command language code))))
       (with-input-from-string (stream string)
         (first (reader:read-forms stream)))))))

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
