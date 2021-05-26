;;;; Copyright (c) 2012-2015 jnjcc, Yste.org. All rights reserved.
;;;;

(in-package #:cl-nlp/test)

(defvar *test-stream* nil)

(defvar *float-zero-epsilon* 1.0e-10)

(defun float= (a b)
  (<= (abs (- a b)) *float-zero-epsilon*))

(defun float/= (a b)
  (> (abs (- a b)) *float-zero-epsilon*))

(defun float< (a b)
  (and (float/= a b)
       (< (- a b) 0)))

(defun make-adjustable-string ()
  (make-array '(0) :element-type 'base-char :fill-pointer 0 :adjustable t))
