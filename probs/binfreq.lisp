;;;; Copyright (c) 2012-2015 jnjcc, Yste.org. All rights reserved.
;;;;
;;;; lightweight frequency distribution using bins

(in-package #:cl-nlp)

(defclass freq-dist-bins (freq-dist)
  ((bins :initform nil)
   (sumfreq :initform nil)
   (binnums :initform nil)))

(defmethod fdist-key ((fdist freq-dist-bins) key)
  (with-slots (test bins) fdist
    (if test
        (assoc key bins :test test)
        (assoc key bins))))

(defmethod for-each-pair ((fdist freq-dist-bins) op)
  (with-slots (bins) fdist
    (dolist (pair bins)
      (funcall op (car pair) (cdr pair)))))

(defmethod fdist-incf ((fdist freq-dist-bins) key &optional (delta 1))
  (with-slots (test bins) fdist
    (if (fdist-key fdist key)
        (if test
            (incf (cdr (assoc key bins :test test)) delta)
            (incf (cdr (assoc key bins)) delta))
        (push (cons key delta) bins))))

(defmethod fdist-setf ((fdist freq-dist-bins) key value)
  (with-slots (test bins) fdist
    (if (fdist-key fdist key)
        (if test
            (setf (cdr (assoc key bins :test test)) value)
            (setf (cdr (assoc key bins)) value)))
    (when (= value 0)
      (setf bins (remove-if (lambda (x) (<= x 0)) bins :key #'cdr)))))

(defmethod fdist-decf ((fdist freq-dist-bins) key &optional (delta 1))
  (when (fdist-key fdist key)
    (with-slots (test bins) fdist
      (if test
          (decf (cdr (assoc key bins :test test)) delta)
          (decf (cdr (assoc key bins)) delta))
      (setf bins (remove-if (lambda (x) (<= x 0)) bins :key #'cdr)))))

(defmethod fdist-sum ((fdist freq-dist-bins))
  (with-slots (sumfreq bins) fdist
    (unless sumfreq
      (setf sumfreq (reduce #'+ (mapcar #'cdr bins) :initial-value 0)))
    sumfreq))

(defmethod fdist-bin ((fdist freq-dist-bins))
  (with-slots (binnums bins) fdist
    (unless binnums
      (setf binnums (length bins)))
    binnums))
