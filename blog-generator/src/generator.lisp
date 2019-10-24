;;; High-level bulk page generation
;;
;; SPDX-License-Identifier: GPL-3.0-or-later
(defpackage #:blog-generator
  (:use #:cl #:iterate #:blog-generator.utils)
  (:local-nicknames
   (#:alx #:alexandria)
   (#:article #:blog-generator.article)
   (#:htsl #:blog-generator.htsl)
   (#:reader #:blog-generator.reader)
   (#:syntax-hl #:blog-generator.syntax-hl)
   (#:template #:blog-generator.template))
  (:export
   #:destination-directory
   #:generate-everything
   #:generate-single-article
   #:get-article-source-paths
   #:get-non-article-page-source-paths
   #:make-default-generator
   #:prepare-destination-directory
   #:reload-article-and-retry
   #:source-directory))
(in-package #:blog-generator)

(declaim (optimize (speed 2) (safety 3) (debug 2) (space 1) (compilation-speed 0)))


;;; Generator protocol

(defgeneric get-template-engine (generator)
  (:documentation "Get the GENERATOR's template engine, see the protocol defined
in `blog-generator.template'."))

(defgeneric source-directory (generator)
  (:documentation
   "Get the source directory of the GENERATOR.
The result shall be an *absolute directory pathname*"))

(defgeneric destination-directory (generator)
  (:documentation
   "Get the destination directory of the GENERATOR.
The result shall be an *absolute directory pathname*"))

(defgeneric get-article-source-paths (generator)
  (:documentation
   "Get the list of article source file pathnames, relative to the
GENERATOR's source directory."))

(defgeneric get-non-article-page-source-paths (generator)
  (:documentation
   "Get the list of *non-article* source file pathnames, relative to the
GENERATOR's source directory.

The difference is that non-article pages don't contribute to the
archive, but are otherwise defined just like regular articles are."))

(defgeneric prepare-destination-directory (generator)
  (:documentation
   "Prepare the GENERATOR's destination directory for output.

For example, the generator may remove the old versions of all generated
artifacts."))


;;; Generator-agnostic page generation

;; These variables are dynamically bound by `generate-everything', to avoid
;; passing around eleventy zillion arguments between functions.
(defvar *current-generator*)
(defvar *source-directory*)
(defvar *destination-directory*)
(defvar *article-paths*)
(defvar *non-article-paths*)
(defvar *template-engine*)
(-type (*source-directory* *destination-directory* *archive-directory*) pathname)
(-type (*article-paths* *non-article-paths*) list)

(-> article-html-path (pathname) pathname)
(defun article-html-path (article-path)
  "Return destination HTML file path corresponding to ARTICLE-PATH.

ARTICLE-PATH and the result are both relative to their corresponding
source and destination directories."
  (make-pathname :type "html" :defaults article-path))

(-> article-domain-relative-path ((nullable pathname)) (nullable string))
(defun domain-relative-path (path)
  "Return the PATH formatted as a domain-relative URL string.

To make the caller's life easier, return nil with nil PATH."
  (when path
    (format nil "/~A" (the string (uiop:unix-namestring path)))))

(defstruct (seq-article
             (:copier nil)
             (:predicate nil)
             (:constructor make-seq-article (article source-path predecessor-path successor-path)))
  "An article tagged with its source path, predecessor, and
successor, for archive building.

The PREDECESSOR-PATH and SUCCESSOR-PATH slots are the relative
pathnames of the *generated HTML files*, or nil if it's the first
(resp. last) article."
  ;; Mutable to allow reloading of the article in a restart.
  (article nil :read-only nil :type article:article)
  (source-path nil :read-only t :type pathname)
  (predecessor-path nil :read-only t :type (nullable pathname))
  (successor-path nil :read-only t :type (nullable pathname)))

(-> load-article (pathname) article:article)
(defun load-article (source-path)
  "Load the article from source-directory-relative SOURCE-PATH."
  (iter
    (restart-case
        (return
          (blog-generator.article:parse-article-forms
           (with-open-file (file-stream (uiop:merge-pathnames* source-path *source-directory*))
             (reader:read-forms file-stream))))
      (reload-article-and-retry ()
        :report "Reload the article's source and retry evaluation."))))

(-> write-html-to (string pathname) (values))
(defun write-html-to (html path)
  "Write HTML, prepending a DOCTYPE, to the file specified by PATH.
It is an error if the file already exists; if it doesn't, it will be
created."
  (with-open-file
      (stream path
              :direction :output
              :if-exists :error
              :if-does-not-exist :create)
    (write-string "<!DOCTYPE html>" stream)
    (write-string html stream))
  (values))

(-> generate-article (seq-article) (values))
(defun generate-article (article)
  "Generate the HTML of the given ARTICLE to the `article-html-path'
of its source path."
  (let* ((source-path (seq-article-source-path article))
         (relative-destination-path (article-html-path source-path))
         (article-html
          (iter
            (restart-case
                (return
                  (template:generate-article-html
                   *template-engine*
                   (seq-article-article article)
                   :canonical-url (domain-relative-path relative-destination-path)
                   :previous-url (domain-relative-path (seq-article-predecessor-path article))
                   :next-url (domain-relative-path (seq-article-successor-path article))))
              (reload-article-and-retry ()
                :report "Reload the article's source and retry evaluation."
                (setf (seq-article-article article) (load-article source-path))))))
         (destination-path
          (uiop:merge-pathnames* relative-destination-path *destination-directory*)))
    (write-html-to article-html destination-path)))

(-> generate-non-articles () (values))
(defun generate-non-articles ()
  (let ((template:*inhibit-publication-date* t))
    ;; Non-articles are just like articles, so reuse article code.
    (iter (for path in *non-article-paths*)
          (generate-article (make-seq-article (load-article path) path nil nil))))
  (values))

(-> generate-articles () list)
(defun generate-articles ()
  "Generate all articles, returning the list of `seq-article' objects sorted
by their article's date, ascending."
  (let ((articles
         (flet
             ((load-raw-article (path) (make-seq-article (load-article path) path nil nil))
              (article-date< (x y)
                (article:date< (article:article-date (seq-article-article x))
                               (article:article-date (seq-article-article y))))
              (make-destination-path (article)
                (article-html-path (seq-article-source-path article)))
              (make-cooked-article (article predecessor-path successor-path)
                (make-seq-article (seq-article-article article)
                                  (seq-article-source-path article)
                                  predecessor-path
                                  successor-path)))
           (declare (ftype (-> (pathname) seq-article) load-raw-article))
           (declare (ftype (-> (seq-article seq-article) boolean) article-date<))
           (declare (ftype (-> (seq-article) pathname) make-destination-path))
           (declare (ftype (-> (seq-article (nullable pathname) (nullable pathname)) seq-article)
                           make-cooked-article))
           (let* ((raw-articles
                   (sort (mapcar #'load-raw-article *article-paths*) #'article-date<))
                  (destination-paths (mapcar #'make-destination-path raw-articles)))
             (mapcar #'make-cooked-article
                     raw-articles
                     (cons nil destination-paths)
                     (append (rest destination-paths) '(nil)))))))
    (mapc #'generate-article articles)
    articles))

(defstruct (quarter
             (:copier nil)
             (:predicate nil)
             (:constructor make-quarter (year quarter)))
  (year nil :read-only t :type unsigned-fixnum)
  (quarter nil :read-only t :type (integer 1 4)))

(-> quarter= (quarter quarter) boolean)
(defun quarter= (x y)
  (and (= (quarter-year x) (quarter-year y))
       (= (quarter-quarter x) (quarter-quarter y))))

(-> date-quarter (article:date) quarter)
(defun date-quarter (date)
  (make-quarter (article:date-year date)
                (1+ (truncate (1- (article:date-month date)) 3))))

(-> quarterly-archive-path (quarter pathname) pathname)
(defun quarterly-archive-path (quarter archives-directory)
  (values
   (uiop:merge-pathnames*
    (format nil "~D-q~D.html" (quarter-year quarter) (quarter-quarter quarter))
    archives-directory)))

(-> generate-quarterly-archive (list quarter pathname) (values))
(defun generate-quarterly-archive (articles quarter archives-directory)
  "Generate a single quarterly archive of ARTICLES in QUARTER."
  (let* ((path (quarterly-archive-path quarter archives-directory))
         (relative-path (uiop:enough-pathname path *destination-directory*))
         (html
          (let ((articles-for-template
                 (iter
                   (for article in articles)
                   (collect
                       (let ((source-path (seq-article-source-path article)))
                         (list (seq-article-article article)
                               (uiop:split-name-type (pathname-name source-path))
                               (domain-relative-path (article-html-path source-path))))))))
            (template:generate-archive-html
             *template-engine*
             articles-for-template
             :year (quarter-year quarter)
             :quarter (quarter-quarter quarter)
             :canonical-url (uiop:unix-namestring relative-path)))))
    (write-html-to html path)))

(-> generate-quarterly-archives (list pathname) list)
(defun generate-quarterly-archives (articles archives-directory)
  "Generate the quarterly archives of ARTICLES.
The archives are written to ARCHIVES-DIRECTORY.
Return a list of quarters for which archives were generated, i.e. those
in which there was at least one published article."
  (let ((current-quarter nil)
        (current-articles '())
        (result '()))
    (flet ((emit-current-quarter ()
             (push current-quarter result)
             (generate-quarterly-archive current-articles current-quarter archives-directory)))
      (declare (ftype (-> () (values)) emit-current-quarter))
      (iter (for article in articles)
            (let ((publication-quarter
                   (date-quarter (article:article-date (seq-article-article article)))))
              (unless (and current-quarter (quarter= current-quarter publication-quarter))
                (when current-quarter (emit-current-quarter))
                (setf current-quarter publication-quarter)
                (setf current-articles '()))
              (push article current-articles))
            (finally (when current-articles (emit-current-quarter)))))
    result))

(-> generate-archives-index (list list pathname) (values))
(defun generate-archives-index (articles quarters archives-directory)
  "Generate the index of the archives, by listing all ARTICLES and all
quarterly archives."
  (let* ((path (uiop:merge-pathnames* "index.html" archives-directory))
         (relative-path (uiop:enough-pathname path *destination-directory*))
         (html
          (let ((quarterly-archives-for-template
                 (iter
                   (for q in quarters)
                   (collect (list
                             (quarter-year q)
                             (quarter-quarter q)
                             (domain-relative-path
                              (uiop:enough-pathname
                               (quarterly-archive-path q archives-directory)
                               *destination-directory*))))))
                (articles-for-template
                 (iter
                   (for a in articles)
                   (collect
                       (cons
                        (seq-article-article a)
                        (domain-relative-path (article-html-path (seq-article-source-path a))))))))
            (template:generate-archive-index-html
             *template-engine*
             quarterly-archives-for-template
             articles-for-template
             :canonical-url (uiop:unix-namestring relative-path)))))
    (write-html-to html path)))

(-> generate-archives (list) (values))
(defun generate-archives (articles)
  "Given ARTICLES, a list of `seq-articles' as returned by
`generate-articles', generate the blog archives."
  (let ((archives-directory (uiop:merge-pathnames* "archives/" *destination-directory*)))
    (uiop:ensure-all-directories-exist (list archives-directory))
    (let ((quarters (generate-quarterly-archives articles archives-directory)))
      (generate-archives-index articles quarters archives-directory))))

(-> generate-single-article (t pathname) (values))
(defun generate-single-article (generator path)
  "Use GENERATOR to regenerate only a single article indicated by PATH.

PATH should be a pathname relative to the source directory of the
GENERATOR.

This is mainly useful for rendering an article while writing, as all it
regenerates is that one article, it doesn't maintain archives nor copy
over static files."
  (let ((*current-generator* generator)
        (*source-directory* (source-directory generator))
        (*destination-directory* (destination-directory generator))
        (*template-engine* (get-template-engine generator)))
    (syntax-hl:with-highlighting-server
      (generate-article
       (make-seq-article (load-article path) path nil nil)))))

(-> generate-everything (t) (values))
(defun generate-everything (generator)
  "Given a GENERATOR, generate all articles, archives and
non-article pages."
  (let ((*current-generator* generator)
        (*source-directory* (source-directory generator))
        (*destination-directory* (destination-directory generator))
        (*article-paths* (get-article-source-paths generator))
        (*non-article-paths* (get-non-article-page-source-paths generator))
        (*template-engine* (get-template-engine generator)))
    (prepare-destination-directory generator)
    (syntax-hl:with-highlighting-server
      (generate-non-articles)
      (let ((articles (generate-articles)))
        ;; The front page is always the latest article.
        (alx:when-let ((latest-article (car (last articles))))
          (let ((destination-relative-path
                 (article-html-path (seq-article-source-path latest-article))))
            (uiop:copy-file
             (uiop:merge-pathnames* destination-relative-path *destination-directory*)
             (uiop:merge-pathnames* "index.html" *destination-directory*))))
        (generate-archives articles)))))


;;; Default generator

(defstruct (default-generator (:copier nil) (:predicate nil) (:constructor %make-default-generator))
  (source-directory
   (error "No source directory specified when creating a generator")
   :read-only t
   :type pathname)
  (destination-directory
   (error "No destination directory specified when creating a generator")
   :read-only t
   :type pathname))

(-> make-default-generator ((or string pathname) (or string pathname)) default-generator)
(defun make-default-generator (source-directory destination-directory)
  "Create a new `default-generator' instance, converting the
SOURCE-DIRECTORY and DESTINATION-DIRECTORY from relative paths or
namestrings to absolute pathnames as necessary."
  (flet ((ensure-absolute-directory-pathname (path)
           (truename (uiop:ensure-directory-pathname path))))
    (declare (ftype (-> ((or string pathname)) pathname) ensure-absolute-directory-pathname))
    (%make-default-generator
     :source-directory (ensure-absolute-directory-pathname source-directory)
     :destination-directory (ensure-absolute-directory-pathname destination-directory))))

(defmethod get-template-engine ((generator default-generator))
  (template:make-default-engine))

(defmethod source-directory ((generator default-generator))
  (default-generator-source-directory generator))

(defmethod destination-directory ((generator default-generator))
  (default-generator-destination-directory generator))

(defmethod get-article-source-paths ((generator default-generator))
  (let ((source-directory (default-generator-source-directory generator)))
    (flet ((relativize-path (path) (uiop:enough-pathname path source-directory)))
      (declare (ftype (-> (pathname) pathname) relativize-path))
      (mapcar #'relativize-path (uiop:directory-files source-directory)))))

(defmethod get-non-article-page-source-paths ((generator default-generator))
  (let ((source-directory (default-generator-source-directory generator)))
    (flet ((relativize-path (path) (uiop:enough-pathname path source-directory)))
      (declare (ftype (-> (pathname) pathname) relativize-path))
      (mapcar #'relativize-path
              (uiop:directory-files (uiop:merge-pathnames* "pages/" source-directory))))))

(-> get-directory-contents (pathname) list)
(defun get-directory-contents (directory)
  "Get the files and subdirectory of the given DIRECTORY.

Note that this function is *not* recursive."
  (values (uiop:directory* (uiop:merge-pathnames* uiop:*wild-file-for-directory* directory))))

(-> clean-up-directory (pathname) (values))
(defun clean-up-directory (directory)
  "Remove all files and subdirectories except top-level dotfiles from
DIRECTORY."
  (flet ((dotfile-name-p (name) (uiop:string-prefix-p "." name)))
    (declare (inline dotfile-name-p) (ftype (-> (string) t) dotfile-name-p))
    (iter (for path in (get-directory-contents directory))
          (if (uiop:directory-pathname-p path)
              (unless (dotfile-name-p (car (last (pathname-directory path))))
                (uiop:delete-directory-tree path :validate t :if-does-not-exist :ignore))
              ;; If the path has no name, it can only be a dotfile.
              (unless (dotfile-name-p (or (pathname-name path) "."))
                (delete-file path)))))
  (values))

(-> copy-subdirectory (string pathname pathname) (values))
(defun copy-subdirectory (subdirectory-name source destination)
  "Copy the subdirectory named SUBDIRECTORY-NAME from SOURCE to DESTINATION.

Copying is done recursively."
  (labels
      ((impl (relative-path)
         (let ((source-path (uiop:merge-pathnames* relative-path source))
               (destination-path (uiop:merge-pathnames* relative-path destination)))
           (if (uiop:directory-pathname-p relative-path)
               (progn
                 (uiop:ensure-all-directories-exist (list destination-path))
                 (iter (for child in (get-directory-contents source-path))
                       (impl (uiop:enough-pathname child source))))
               (uiop:copy-file source-path destination-path)))
         (values)))
    (declare (ftype (-> (pathname) (values)) impl))
    (impl (uiop:ensure-directory-pathname subdirectory-name))))

(defmethod prepare-destination-directory ((generator default-generator))
  "Prepare the destination directory by removing all files and
directories except top-level dotfiles, to avoid messing up VCS files,
then copying the \"static\" subdirectory of the source directory and all
of its contents to the destination."
  (let ((source (default-generator-source-directory generator))
        (destination (default-generator-destination-directory generator)))
    (clean-up-directory destination)
    (copy-subdirectory "static" source destination)
    (uiop:ensure-all-directories-exist (list (uiop:merge-pathnames* "pages/" destination)))))
