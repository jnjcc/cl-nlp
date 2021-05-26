;;;; Copyright (c) 2012-2015 jnjcc, Yste.org. All rights reserved.
;;;;
;;;; Conditional Probability Distribution from Conditional Frequency Distribution

(in-package #:cl-nlp)

(defclass cond-freq-dist (freq-dist-hash)
  ())

(defmethod fdist-cond-value ((cfdist cond-freq-dist) condi key)
  (let ((fdist (fdist-value cfdist condi)))
    (when fdist
      (fdist-value fdist key))))

(defmethod fdist-cond-freq ((cfdist cond-freq-dist) condi key)
  "freq(key | condi), will return 0 when `fdist-cond-value' returns nil"
  (let ((fdist (fdist-value cfdist condi)))
    (if fdist
        (fdist-freq fdist key)
        0)))

(defmethod fdist-cond-incf ((cfdist cond-freq-dist) condi key &optional (delta 1))
  (with-slots (test htable) cfdist
    (let ((fdist (fdist-value cfdist condi)))
      (unless fdist
        (setf fdist (make-freq-dist :hash :test test))
        (fdist-setf cfdist condi fdist))
      (fdist-incf fdist key delta)
      )))

(defmethod fdist-cond-setf ((cfdist cond-freq-dist) condi key value)
  (with-slots (test htable) cfdist
    (let ((fdist (fdist-value cfdist condi)))
      (unless fdist
        (setf fdist (make-freq-dist :hash :test test))
        (fdist-setf cfdist condi fdist))
      (fdist-setf fdist key value))))

;; how many (`condi', `key') pairs are there?
(defmethod fdist-cond-bins ((cfdist cond-freq-dist))
  (let ((bins 0)
        (fdist nil))
    (dolist (condi (fdist-keys cfdist))
      (setf fdist (fdist-value cfdist condi))
      (when fdist
        (incf bins (fdist-bin fdist))))
    bins))

;;; Derived Probability Distribution, with smoothing
(defmethod fdist-cond-sum ((cfdist cond-freq-dist) condi)
  (let ((fdist (fdist-value cfdist condi)))
    (fdist-sum fdist)))

;; loop through all (`condi', `key', `value') pairs such that CF(`key'|`condi')=`value'
;; TODO: this macro cannot do (RETURN) right
(defmacro do-cond-pairs ((condi key value cfdist) &body body)
  `(let ((,value nil))
     (dolist (,condi (fdist-keys ,cfdist))
       (dolist (,key (fdist-keys (fdist-value ,cfdist ,condi)))
         (setf ,value (fdist-cond-value ,cfdist ,condi ,key))
         ,@body))))

;; Given `condi', loop through all (`key', `value') pairs such that CP(`key'|`condi')=`value'
(defmacro do-cond-values ((key value condi cfdist) &body body)
  `(let ((,value nil))
     (dolist (,key (fdist-keys (fdist-value ,cfdist ,condi)))
       (setf ,value (fdist-cond-value ,cfdist ,condi ,key))
       ,@body)))

;;; make-* functions
(defun make-cond-freq (&key (test nil))
  (make-instance 'cond-freq-dist :test test))
