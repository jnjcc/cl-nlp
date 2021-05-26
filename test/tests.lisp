;;;; Copyright (c) 2012-2015 jnjcc, Yste.org. All rights reserved.
;;;;

(in-package #:cl-nlp/test)

(defun run-all-tests ()
  (let ((result (run-tests :all :cl-nlp/test)))
    (test-names result)
    result))
