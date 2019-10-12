;; SPDX-License-Identifier: GPL-3.0-or-later
(defpackage blog-generator.htsl.test
  (:use #:cl
        #:rove
        #:blog-generator.htsl))
(in-package #:blog-generator.htsl.test)

(define-tag-macro test-scaffolding (attributes &rest children)
  (declare (ignore attributes))
  `(html
    (head
     (title "Test."))
    (body
     ,@children)))

(deftest root-elements
  (testing "html is a valid root"
    (ok (convert-document '(html)))
    (ok (convert-document '((html :lang "en")))))
  (testing "non-html elements aren't valid roots"
    (ok (signals (convert-document '(section)) 'nesting-error))
    (ok (signals (convert-document '(a)) 'nesting-error))
    (ok (signals (convert-document "text") 'nesting-error))))

(deftest nesting
  (testing "paragraphs only accept phrasing elements"
    (ok (convert-document '(test-scaffolding (p "foo" ((a :href "#") "bar") (strong (em "bar"))))))
    (ok (signals (convert-document '(test-scaffolding (p "foo" (p "bar")))) 'nesting-error))
    (ok (signals (convert-document '(test-scaffolding (p (ul (li "wrong"))))) 'nesting-error)))
  (testing "lists only accepts list items"
    (ok (convert-document '(test-scaffolding (ul (li "foo") (li "bar")))))
    (ok (signals (convert-document '(test-scaffolding (li "qux"))) 'nesting-error))
    (ok (signals (convert-document '(test-scaffolding (ul (p "test")))) 'nesting-error))))

(deftest attributes
  (testing "global attributes work on all tags"
    (ok (convert-document '((html :class "foo"))))
    (ok (convert-document '(test-scaffolding ((p :id "bar") "test")))))
  (testing "tag-specific attributes work only on their respective tags"
    (ok (convert-document '(test-scaffolding (p ((a :href "#") "test")))))
    (ok (signals (convert-document '(test-scaffolding ((p :href "#") "test"))) 'unknown-attribute)))
  (testing "data-* attributes are allowed everywhere"
    (ok (convert-document '((html :data-foo "bar"))))
    (ok (convert-document '(test-scaffolding ((p :data-bar "qux"))))))
  (testing "attributes are typed"
    (ok (convert-document '((html :lang "en"))))
    (ok (signals (convert-document '((html :lang 123))) 'wrong-attribute-type))
    (ok (convert-document '(test-scaffolding ((ol :reversed t)))))
    (ok (signals (convert-document '(test-scaffolding ((ol :reversed "yes")))) 'wrong-attribute-type))
    (ok (convert-document '(test-scaffolding ((ol :start 15)))))
    (ok (signals (convert-document '(test-scaffolding ((ol :start nil)))) 'wrong-attribute-type))))

(deftest escaping
  (testing "Attributes are escaped correctly"
    (ok (string= "<html class=\"&lt;&gt;&amp;&quot;\"></html>"
                 (convert-document '((html :class "<>&\""))))))
  (testing "Text nodes are escaped correctly"
    (ok (string= "<html><head><title>Test.</title></head><body><p>&lt;&gt;\"&amp;</p></body></html>"
                 (convert-document '(test-scaffolding (p "<>\"&"))))))
  (testing "There are no unneeded entities"
    (ok (string= "<html class=\"foo\"></html>"
                 (convert-document '((html :class "foo")))))))

(deftest closing-tags
  (testing "Closing tags are omitted where appropriate"
    (ok (string= "<html><head><meta charset=\"UTF-8\"></head></html>"
                 (convert-document '(html (head ((meta :charset "UTF-8")))))))))

(deftest malformed-inputs
  (testing "Unknown tags raise a signal"
    (ok (signals (convert-document '(foo)) 'unknown-tag)))
  (testing "Unparseable sexps raise a singal"
    (ok (signals (convert-document #(a vector)) 'invalid-sexp)))
  (testing "Unparseable tag heads raise a signal"
    (ok (signals (convert-document '(html (#(a vector again) "foo"))) 'invalid-tag-head))))
