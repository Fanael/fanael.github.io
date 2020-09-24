;;; HTML tag definitions
;; Copyright © 2019  Fanael Linithien
;; SPDX-License-Identifier: GPL-3.0-or-later
(defpackage #:blog-generator.tags
  (:use #:cl #:iterate #:blog-generator.htsl))
(in-package #:blog-generator.tags)

(declaim (optimize (speed 2) (safety 3) (debug 2) (space 1) (compilation-speed 0)))

(define-tag html
    :allowed-contexts :root
    :child-context :head-and-body)

(define-tag head
    :allowed-contexts :head-and-body
    :child-context :head)

(define-tag meta
    :allowed-contexts :head
    :child-context nil
    :attributes '((:charset . string)
                  (:content . string)
                  (:http-equiv . string)
                  (:name . string))
    :omit-closing-tag t)

(define-tag link
    :allowed-contexts :head
    :child-context nil
    :attributes '((:href . string)
                  (:rel . string))
    :omit-closing-tag t)

(define-tag title
    :allowed-contexts :head
    :child-context :text-only)

(define-tag body
    :allowed-contexts :head-and-body
    :child-context :flow)

(macrolet
    ((define-simple-flow-tags (&rest tag-names)
       `(progn
          ,@(iter (for tag-name in tag-names)
                  (collect `(define-tag ,tag-name
                                :allowed-contexts :flow
                                :child-context :flow))))))
  (define-simple-flow-tags main article section nav header footer aside)
  (define-simple-flow-tags h1 h2 h3 h4 h5 h6)
  (define-simple-flow-tags blockquote div))

(define-tag p
    :allowed-contexts :flow
    :child-context :phrasing)

(define-tag hr
    :allowed-contexts :flow
    :child-context nil)

(define-tag pre
    :allowed-contexts :flow
    :child-context :phrasing)

(define-tag ol
    :allowed-contexts :flow
    :child-context :list-element
    :attributes '((:reversed . boolean)
                  (:start . integer)
                  (:type . string)))

(define-tag ul
    :allowed-contexts :flow
    :child-context :list-element)

(define-tag li
    :allowed-contexts :list-element
    :child-context :flow)

(define-tag-macro unordered-list (attributes children)
  `((ul ,@attributes)
    ,@(iter (for child in children) (collect `(li ,child)))))

(define-tag-macro ordered-list (attributes children)
  `((ol ,@attributes)
    ,@(iter (for child in children) (collect `(li ,child)))))


(define-tag a
    :allowed-contexts '(:flow :phrasing)
    :child-context #'identity
    :attributes '((:href . string)
                  (:rel . string)))

(macrolet
    ((define-simple-phrasing-tags (&rest tag-names)
       `(progn
          ,@(iter (for tag-name in tag-names)
                  (collect `(define-tag ,tag-name
                                :allowed-contexts '(:flow :phrasing)
                                :child-context :phrasing))))))
  (define-simple-phrasing-tags em strong code var samp kbd span b i sup sub cite dfn))

(define-tag time
    :allowed-contexts '(:flow :phrasing)
    :child-context :phrasing
    :attributes '((:datetime . string)))

(define-tag figure
    :allowed-contexts :flow
    :child-context :figure)

(define-tag figcaption
    :allowed-contexts :figure
    :child-context :flow)

(define-tag img
    :allowed-contexts :figure
    :child-context nil
    :attributes '((:src . string)
                  (:alt . string)
                  (:height . integer)
                  (:width . integer))
    :omit-closing-tag t)

(define-tag-macro image-figure (attributes children)
  `(figure
    ((img ,@attributes))
    (figcaption ,@children)))
