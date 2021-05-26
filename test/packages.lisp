;;;; Copyright (c) 2012-2015 jnjcc, Yste.org. All rights reserved.
;;;;

(in-package #:cl-user)

(defpackage #:cl-nlp/test
  (:nicknames #:nlp/test)
  (:use #:cl #:lisp-unit #:cl-nlp/algo #:cl-nlp
        #:cl-ml/io)
  (:export #:run-all-tests))
