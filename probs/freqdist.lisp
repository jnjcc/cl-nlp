;;;; Copyright (c) 2012-2015 jnjcc, Yste.org. All rights reserved.
;;;;
;;;; Frequency Distribution

(in-package #:cl-nlp)

;;; <key, value> pairs, where value not necessarily being frequency
(defclass freq-dist ()
  ((test :initform nil :initarg :test)))

(defgeneric fdist-key (fdist key)
  (:documentation "returns (key . freq) or nil if not exists"))

(defmethod fdist-value ((fdist freq-dist) key)
  "return value associated with `key', not necessarily frequency"
  (cdr (fdist-key fdist key)))

(defmethod fdist-freq ((fdist freq-dist) key)
  "return freq associated with `key'"
  (let ((pair (fdist-key fdist key)))
    (if pair
        (cdr pair)
        0)))

(defgeneric for-each-pair (fdist op)
  (:documentation "do `op' on each (key . value) pair"))

(defmethod fdist-keys ((fdist freq-dist))
  "return keys into a list"
  (let ((keylst nil))
    (labels ((collect (key value)
               (declare (ignore value))
               (push key keylst)))
      (for-each-pair fdist #'collect))
    keylst))

(defgeneric fdist-incf (fdist key &optional delta)
  (:documentation "incf value associated with `key' by `delta'"))

(defgeneric fdist-setf (fdist key value)
  )

(defgeneric fdist-decf (fdist key &optional delta)
  (:documentation "decf value associated with `key' by `delta'"))

(defgeneric fdist-count (fdist lst &optional start end)
  (:documentation "count frequency distribution from `lst'"))

(defmethod fdist-count ((fdist freq-dist) lst &optional (start 0) end)
  (unless end
    (setf end (length lst)))
  (do ((i start (1+ i)))
      ((>= i end))
    (fdist-incf fdist (nth i lst))))

;;; Derived Probability Distribution, with smoothing
(defgeneric fdist-sum (fdist)
  (:documentation "total frequency of all keys, i.e., N
NOTICE: only called when we are sure there will be no more `fdist-incf'"))

(defgeneric fdist-bin (fdist)
  (:documentation "number of keys, i.e., B
NOTICE: only called when we are sure there will be no more `fdist-incf'"))

;; loop through all (`key', `value') pairs such that F(`key')=`value'
(defmacro do-fdist-pairs ((key value fdist) &body body)
  `(let ((,value nil))
     (dolist (,key (fdist-keys ,fdist))
       (setf ,value (fdist-value ,fdist ,key))
       ,@body)))

;;; make-* functions
(defun make-freq-dist (type &key (test nil))
  (ecase type
    (:bin (make-instance 'freq-dist-bins :test test))
    (:hash (make-instance 'freq-dist-hash :test test))))
