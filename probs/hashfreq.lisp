;;;; Copyright (c) 2012-2015 jnjcc, Yste.org. All rights reserved.
;;;;
;;;; Frequency Distribution using hash-table

(in-package #:cl-nlp)

(defclass freq-dist-hash (freq-dist)
  ((htable :initform nil)
   (sumfreq :initform nil)))

(defmethod initialize-instance :after ((fdist freq-dist-hash) &rest args)
  (declare (ignore args))
  (with-slots (test htable) fdist
    (unless test
      (setf test 'eql))
    (setf htable (make-hash-table :test test))))

(defmethod fdist-key ((fdist freq-dist-hash) key)
  (with-slots (htable) fdist
    (multiple-value-bind (value exist) (gethash key htable)
      (when exist
        (cons key value)))))

(defmethod for-each-pair ((fdist freq-dist-hash) op)
  (with-slots (htable) fdist
    (maphash (lambda (key value) (funcall op key value)) htable)))

(defmethod fdist-incf ((fdist freq-dist-hash) key &optional (delta 1))
  (with-slots (htable) fdist
    (multiple-value-bind (value exist) (gethash key htable)
      (unless exist
        (setf value 0))
      (incf value delta)
      (setf (gethash key htable) value))))

(defmethod fdist-setf ((fdist freq-dist-hash) key value)
  (with-slots (htable) fdist
    (setf (gethash key htable) value)))

(defmethod fdist-decf ((fdist freq-dist-hash) key &optional (delta 1))
  (with-slots (htable) fdist
    (multiple-value-bind (value exist) (gethash key htable)
      (when exist
        (decf value delta)
        (if (<= value 0)
            (remhash key htable)
            (setf (gethash key htable) value))))))

(defmethod fdist-sum ((fdist freq-dist-hash))
  (with-slots (sumfreq htable) fdist
    (unless sumfreq
      (let ((freq 0))
        (labels ((collect (key value)
                   (declare (ignore key))
                   (incf freq value)))
          (maphash #'collect htable))
        (setf sumfreq freq)))
    sumfreq))

(defmethod fdist-bin ((fdist freq-dist-hash))
  (with-slots (htable) fdist
    (hash-table-count htable)))
