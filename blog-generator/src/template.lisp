;;; HTML templating
;; Copyright © 2019-2020  Fanael Linithien
;; SPDX-License-Identifier: GPL-3.0-or-later OR CC-BY-SA-4.0
(defpackage #:blog-generator.template
  (:use #:cl #:iterate #:blog-generator.utils)
  (:local-nicknames
   (#:alx #:alexandria)
   (#:article #:blog-generator.article)
   (#:htsl #:blog-generator.htsl)
   (#:syntax-hl #:blog-generator.syntax-hl))
  (:export
   #:*current-engine*
   #:*inhibit-publication-date*
   #:*topic-to-archive-function*
   #:archived-article
   #:archived-article-article
   #:archived-article-id
   #:archived-article-url
   #:generate-archive-index-html
   #:generate-archive-index-htsl
   #:generate-article-html
   #:generate-article-htsl
   #:generate-quarterly-archive-html
   #:generate-quarterly-archive-htsl
   #:generate-topic-archive-html
   #:generate-topic-archive-htsl
   #:make-archived-article
   #:make-default-engine
   #:make-quarterly-archive
   #:make-topic-archive
   #:quarterly-archive
   #:quarterly-archive-quarter
   #:quarterly-archive-url
   #:quarterly-archive-year
   #:topic-archive
   #:topic-archive-topic
   #:topic-archive-url
   #:wrap-highlighted-code))
(in-package #:blog-generator.template)

(declaim (optimize (speed 2) (safety 3) (debug 2) (space 1) (compilation-speed 0)))


;;; Template engine protocol

(define-immutable-structure archived-article ((make-archived-article (article id url)))
  (article :type article:article)
  (id :type string)
  (url :type string))

(define-immutable-structure quarterly-archive ((make-quarterly-archive (year quarter url)))
  (year :type unsigned-fixnum)
  (quarter :type (integer 1 4))
  (url :type string))

(define-immutable-structure topic-archive ((make-topic-archive (topic url)))
  (topic :type string)
  (url :type string))

(defvar *current-engine* nil
  "The current templating engine, dynamically bound to allow use from
within HTSL tag macros.")

(defvar *inhibit-publication-date* nil
  "If non-nil, the publication date will be omitted from the output.")

(-type *topic-to-archive-function* (nullable (function (string) string)))
(defvar *topic-to-archive-function* nil
  "The function converting an article topic name to an URL to the
corresponding topic archive page.

May be nil, in which case no links will be created.")

(defgeneric generate-article-htsl (engine article canonical-url previous-url next-url)
  (:documentation "Use ENGINE to generate the whole document HTSL of the
given ARTICLE, which is an `article:article' object.

CANONICAL-URL is the string representing the canonical URL of the
article for the purposes of <link rel=\"canonical\">.

If PREVIOUS-URL and/or NEXT-URL is non-nil, it's the domain-relative
URLs of the previous (resp. next) blog article."))

(defgeneric generate-quarterly-archive-htsl (engine articles year quarter canonical-url)
  (:documentation "Use ENGINE to generate the whole document HTSL of a
single quarterly archive page.

ARTICLES is a list of `archived-article' objects.

CANONICAL-URL is the string representing the canonical URL of the page
for the purposes of <link rel=\"canonical\">."))

(defgeneric generate-topic-archive-htsl (engine articles topic canonical-url)
  (:documentation "Use ENGINE to generate the whole document HTSL of a
single topic archive page.

ARTICLES is a list of `archived-article' objects.

CANONICAL-URL is the string representing the canonical URL of the page
for the purposes of <link rel=\"canonical\">."))

(defgeneric generate-archive-index-htsl (engine quarters topics articles canonical-url)
  (:documentation "Use ENGINE to generate the whole document HTSL of the
archive index page.

QUARTERS is a list of `quarterly-archive' objects.
TOPICS is a list of `topic-archive' objects.
ARTICLES is a list of `archived-article' objects.

CANONICAL-URL is the string representing the canonical URL of the page
for the purposes of <link rel=\"canonical\">."))

(defgeneric wrap-highlighted-code (engine language-pretty-name htsl-forms)
  (:documentation "Wrap syntax-highlighted code in an ENGINE-dependent manner.

LANGUAGE-PRETTY-NAME is a string representing \"pretty\" human-readable
name of the programming language."))

(-> generate-article-html
    (t
     article:article
     &key
     (:canonical-url string)
     (:previous-url (nullable string))
     (:next-url (nullable string)))
    string)
(defun generate-article-html (engine article &key canonical-url previous-url next-url)
  "Use `generate-article-htsl' to generate the article HTSL, then
convert it to HTML.

This function is the preferred way of using a templating engine to
generate an article, as it allows HTSL tag macros to access the current
templating engine."
  (let ((*current-engine* engine))
    (htsl:convert-document
     (generate-article-htsl engine article canonical-url previous-url next-url))))

(-> generate-archive-html
    (t
     list
     &key
     (:year unsigned-fixnum)
     (:quarter (integer 1 4))
     (:canonical-url string))
    string)
(defun generate-quarterly-archive-html (engine articles &key year quarter canonical-url)
  "Use `generate-quarterly-archive-htsl' to generate the quarterly
archive page HTSL, then convert it to HTML.

This function is the preferred way of using a templating engine to
generate an archive page, as it allows HTSL tag macros to access the
current templating engine."
  (let ((*current-engine* engine))
    (htsl:convert-document
     (generate-quarterly-archive-htsl engine articles year quarter canonical-url))))

(-> generate-topic-archive-html (t list &key (:topic string) (:canonical-url string)) string)
(defun generate-topic-archive-html (engine articles &key topic canonical-url)
  "Use `generate-topic-archive-htsl' to generate the topic archive page
HTSL, then convert it to HTML.

This function is the preferred way of using a templating engine to
generate an archive page, as it allows HTSL tag macros to access the
current templating engine."
  (let ((*current-engine* engine))
    (htsl:convert-document
     (generate-topic-archive-htsl engine articles topic canonical-url))))

(-> generate-archive-index-html (t list list list &key (:canonical-url string)) string)
(defun generate-archive-index-html (engine quarters topics articles &key canonical-url)
  "Use `generate-archive-index-htsl' to generate the archive index HTSL,
then convert it to HTML.

This function is the preferred way of using a templating engine to
generate the archive index page, as it allows HTSL tag macros to access
the current templating engine."
  (let ((*current-engine* engine))
    (htsl:convert-document
     (generate-archive-index-htsl engine quarters topics articles canonical-url))))


;;; Syntax highlighting support

(alx:define-constant +syntax-highlighting-languages+
    '((:c++ "c++" . "C++")
      (:common-lisp "common-lisp" . "Common Lisp")
      (:java "java" . "Java"))
  :test #'equal
  :documentation "List of known syntax highlighting languages.
Each element is of the form (KEYWORD PYGMENTS-NAME . PRETTY-NAME).")

(htsl:define-tag-macro highlighted-code (attributes children)
  (destructuring-bind (&key language) attributes
    (trivia:let-match
        (((list* _ pygments-name pretty-name)
          (assoc language +syntax-highlighting-languages+ :test #'eq))
         ((list (and (type string) code)) children))
      (wrap-highlighted-code *current-engine*
                             pretty-name
                             (syntax-hl:highlight-code code pygments-name)))))


;;; Default template engine

(eval-and-compile
  (define-immutable-structure default-engine ((%make-default-engine ()))))

(eval-and-compile
  (defmethod make-load-form ((self default-engine) &optional environment)
    (declare (ignore environment))
    '(%make-default-engine)))

(alx:define-constant +default-engine+ (%make-default-engine)
  :test (constantly t)
  :documentation "The single instance of the (stateless) default engine.")

(-> make-default-engine () default-engine)
(defun make-default-engine ()
  "Make an instance of the default templating engine."
  +default-engine+)

(defmethod wrap-highlighted-code ((engine default-engine) pretty-name forms)
  `((pre :class "codeblock" :data-code-language ,pretty-name)
    (code ,@forms)))

(-> generate-htsl-head (string string &optional (nullable string)) list)
(defun generate-htsl-head (canonical-url title &optional description)
  "Given a page's CANONICAL-URL, TITLE and an optional DESCRIPTION,
generate the head section of the HTML document in HTSL form."
  `(head
    ((meta :charset "UTF-8"))
    ((meta :name "viewport" :content "width=device-width, initial-scale=1"))
    ((meta :http-equiv "Content-Security-Policy" :content "default-src 'self'"))
    ((meta :name "generator" :content "Some custom Common Lisp"))
    ,@(when description
        `(((meta :name "description" :content ,description))))
    ((link :rel "canonical" :href ,canonical-url))
    ((link :rel "stylesheet" :href "/static/theme.css"))
    ((link :rel "license" :href "https://creativecommons.org/licenses/by-sa/4.0/"))
    (title ,(format nil "~A - Fanael's random ruminations" title))))

(alx:define-constant +main-header-htsl+
    '((header :id "mainheader") "Fanael's random ruminations")
  :test #'equal
  :documentation "The main header (banner) in HTSL form.")

(alx:define-constant +nav-menu-htsl+
    '(nav
      ((unordered-list :id "navmenu")
       ((a :href "/") "Main page")
       ((a :href "/archives/") "Archives")
       ((a :href "https://github.com/Fanael/fanael.github.io/") "GitHub")
       ((a :rel "author" :href "/pages/about.html") "About")))
  :test #'equal
  :documentation "The navigation menu under the main header, in HTSL form.")

(alx:define-constant +article-root-id+ "root-section"
  :test #'string=
  :documentation "The ID of the root section of an article.")

(-> generate-section-header-link (string) list)
(eval-and-compile
  (defun generate-section-header-link (section-href)
    "Generate a section header link targeting SECTION-HREF."
    `((a :class "section-header-link" :href ,section-href) "§")))

(alx:define-constant +article-root-header-link+
    (generate-section-header-link (format nil "#~A" +article-root-id+))
  :test #'equal
  :documentation "The section header link for the root section of an article.")

(alx:define-constant +month-names+
    #("January" "February" "March" "April" "May" "June"
      "July" "August" "September" "October" "November" "December")
  :test #'equalp)

(-> generate-iso-date (article:date) string)
(defun generate-iso-date (date)
  "Convert DATE into YYYY-MM-DD formatted string."
  (trivia:let-match1 (article:date year month day) date
    (format nil "~D-~2,'0d-~2,'0d" year month day)))

(-> generate-pretty-date (article:date) list)
(defun generate-pretty-date (date)
  "Pretty-print DATE into HTSL, returning a (machine-readable) `time'
tag containing (human-readable) English."
  (trivia:let-match1 (article:date year month day) date
    (let ((pretty-date
           (format nil "~D~A of ~A ~D"
                   day
                   (trivia:match (mod day 10)
                     (1 (if (= day 11) "th" "st"))
                     (2 (if (= day 12) "th" "nd"))
                     (3 (if (= day 13) "th" "rd"))
                     (_ "th"))
                   (the string (aref +month-names+ (1- month)))
                   year)))
      `((time :datetime ,(generate-iso-date date)) ,pretty-date))))

(-> generate-publication-date (article:date) list)
(defun generate-publication-date (date)
  "Generate markup for the publication DATE of an article."
  `((p :class "publishdate")
    "Published on the "
    ,(generate-pretty-date date)))

(-> generate-article-topics (list) list)
(defun generate-article-topics (topics)
  "Generate markup for the list of article TOPICS."
  (flet ((make-link-to-archive-if-possible (topic)
           (alx:if-let ((func *topic-to-archive-function*))
             `((a :href ,(funcall func topic)) ,topic)
             topic)))
    `((p :class "article-topics")
      "Topics: "
      ,@(iter (for (topic . rest) on topics)
              (collect (make-link-to-archive-if-possible topic))
              (when rest (collect ", "))))))

(-> generate-article-header (article:article) list)
(defun generate-article-header (article)
  "Generate the article header, i.e. the article title inside `h1', the
article topics and the article's publication date."
  `(header
    (h1
     ,+article-root-header-link+
     ,(article:article-title article))
    ,@(unless *inhibit-publication-date*
        (list (generate-publication-date (article:article-date article))))
    ,@(alx:when-let ((topics (article:article-topics article)))
        (list (generate-article-topics topics)))))

(alx:define-constant +footer-htsl+
    '(footer
      ((ul :id "footerstuff")
       (li "Powered by HTML & CSS")
       (li "Copyright © 2019-2020 Fanael Linithien")
       (li "Licensed under a "
        ((a :rel "license" :href "https://creativecommons.org/licenses/by-sa/4.0/")
         "Creative Commons Attribution-ShareAlike 4.0 International License"))))
  :test #'equal
  :documentation "The common footer, in HTSL form.")

(-> generate-bottom-nav ((nullable string) (nullable string)) list)
(defun generate-bottom-nav (previous-url next-url)
  `(nav
    ((ul :id "prevnext")
     ((li :class "top") ((a :href "#") "↑ Top ↑"))
     ((li :class "prev")
      ,@(when previous-url
          `(((a :rel "prev" :href ,previous-url) "← Older"))))
     (li ((a :href "/archives/") "Blog archives"))
     ((li :class "next")
      ,@(when next-url
          `(((a :rel "next" :href ,next-url) "Newer →")))))))

(defmethod generate-article-htsl
    ((engine default-engine) article canonical-url previous-url next-url)
  `((html :lang "en")
    ,(generate-htsl-head
      canonical-url
      (article:article-title article)
      (article:article-description article))
    (body
     ,+main-header-htsl+
     ,+nav-menu-htsl+
     (main
      ((article :id ,+article-root-id+)
       ,(generate-article-header article)
       ,@(flet ((wrap-table-of-contents (form)
                  `((nav :class "toc") ,form))
                (add-section-header-link (section form)
                  `(,(first form)
                     ,(generate-section-header-link (article:section-href section))
                     ,@(rest form))))
           (let ((article:*table-of-contents-hook* #'wrap-table-of-contents)
                 (article:*section-heading-hook* #'add-section-header-link))
             (article:generate-body-htsl article)))))
     ,(generate-bottom-nav previous-url next-url)
     ,+footer-htsl+)))

(-> generate-pretty-quarter (unsigned-fixnum (integer 1 4)) string)
(defun generate-pretty-quarter (year quarter)
  "Pretty-print the YEAR and QUARTER into human-readable English."
  (format nil "~A quarter of ~D"
          (the string (aref #("first" "second" "third" "fourth") (1- quarter)))
          year))

(-> generate-archive-toc (list) list)
(defun generate-archive-toc (articles)
  "Given a list of ARTICLES as for `generate-archives-htsl', generate a
table of contents linking each article title to the corresponding
excerpt."
  `((nav :class "toc")
    (ordered-list
     ,@(iter (for article in articles)
             (trivia:let-match1 (archived-article article id _) article
               (collect `((a :href ,(format nil "#~A" id))
                          ,(article:article-title article))))))))

(-> generate-excerpts (list) list)
(defun generate-excerpts (articles)
  "Given a list of ARTICLES as for `generate-archives-htsl', generate a
list of excerpts of each article."
  (iter (for article in articles)
        (trivia:let-match1 (archived-article article* id url) article
          (collect
              `((article :id ,id)
                (header
                 (h2 ((a :href ,url)
                      ,(article:article-title article*)))
                 ,(generate-publication-date (article:article-date article*)))
                ,@(article:section-body (article:article-root-section article*)))))))

(defmethod generate-quarterly-archive-htsl
    ((engine default-engine) articles year quarter canonical-url)
  (let ((pretty-quarter (generate-pretty-quarter year quarter)))
    `((html :lang "en")
      ,(generate-htsl-head
        canonical-url
        (format nil "Blog archives for the ~A" pretty-quarter))
      (body
       ,+main-header-htsl+
       ,+nav-menu-htsl+
       (main
        (header (h1 "Archives for the " ,pretty-quarter))
        ,(generate-archive-toc articles)
        ,@(generate-excerpts articles))
       ,(generate-bottom-nav nil nil)
       ,+footer-htsl+))))

(defmethod generate-topic-archive-htsl ((engine default-engine) articles topic canonical-url)
  `((html :lang "en")
    ,(generate-htsl-head
      canonical-url
      (format nil "Blog archives for topic ~A" (the string topic)))
    (body
     ,+main-header-htsl+
     ,+nav-menu-htsl+
     (main
      (header (h1 "Archives for topic " ,topic))
      ,(generate-archive-toc articles)
      ,@(generate-excerpts articles))
     ,(generate-bottom-nav nil nil)
     ,+footer-htsl+)))

(defmethod generate-archive-index-htsl
    ((engine default-engine) quarters topics articles canonical-url)
  `((html :lang "en")
    ,(generate-htsl-head
      canonical-url
      "Blog archive index")
    (body
     ,+main-header-htsl+
     ,+nav-menu-htsl+
     (main
      (header (h1 "Blog archives"))
      (section
       (h2 "By date")
       (unordered-list
        ,@(iter (for archive in quarters)
                (trivia:let-match1 (quarterly-archive year quarter url) archive
                  (collect `((a :href ,url) "The " ,(generate-pretty-quarter year quarter)))))))
      (section
       (h2 "By topic")
       (unordered-list
        ,@(iter (for archive in topics)
                (trivia:let-match1 (topic-archive topic url) archive
                  (collect `((a :href ,url) ,topic))))))
      (section
       (h2 "By article title")
       (unordered-list
        ,@(iter (for article in (reverse articles))
                (trivia:let-match1 (archived-article article _ url) article
                  (collect
                      `((a :href ,url)
                        ,(generate-iso-date (article:article-date article))
                        " — "
                        ,(article:article-title article))))))))
     ,(generate-bottom-nav nil nil)
     ,+footer-htsl+)))
