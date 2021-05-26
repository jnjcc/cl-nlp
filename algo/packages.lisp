;;;; Copyright (c) 2012-2015 jnjcc, Yste.org. All rights reserved.
;;;;
;;;; Common data structure and algorithms for NLP

(in-package #:cl-user)

(defpackage #:cl-nlp/algo
  (:nicknames #:nlp/algo)
  (:use #:cl)
  (:export #:edit-distance
           #:make-adict #:zip-adict #:adict-len #:adict-add #:adict-append #:adict-ref
           #:make-hdict #:zip-hdict #:hdict-len #:hdict-add #:hdict-append #:hdict-ref

           #:make-badict #:zip-badict #:badict-len #:badict-add #:badict-ref #:badict-bref
           #:make-bhdict #:zip-bhdict #:bhdict-len #:bhdict-add #:bhdict-ref #:bhdict-bref
           ))
