;; SPDX-License-Identifier: GPL-3.0-or-later
(defsystem "blog-generator"
  :version "0.1.0"
  :author "Fanael Linithien"
  :license "GPL-3.0-or-later"
  :depends-on ("alexandria"
               "iterate"
               "trivia"
               "uiop")
  :components ((:module "src"
                :components
                ((:static-file "pygments_server.py")
                 (:file "utils")
                 (:file "reader" :depends-on ("utils"))
                 (:file "htsl" :depends-on ("utils"))
                 (:file "tags" :depends-on ("utils" "htsl"))
                 (:file "article" :depends-on ("utils" "reader"))
                 (:file "syntax-hl" :depends-on ("utils" "reader"))
                 (:file "template" :depends-on ("utils" "htsl" "article" "syntax-hl"))
                 (:file "generator" :depends-on ("utils" "htsl" "article" "template")))))
  :description ""
  :around-compile "(lambda (next) (let ((iterate::*always-declare-variables* t)) (funcall next)))"
  :in-order-to ((test-op (test-op "blog-generator/tests"))))

(defsystem "blog-generator/tests"
  :author "Fanael Linithien"
  :license "GPL-3.0-or-later"
  :depends-on ("blog-generator"
               "rove")
  :components ((:module "tests"
                :components
                ((:file "htsl"))))
  :description "Test system for blog-generator"
  :perform (test-op (op c) (symbol-call :rove :run c)))
