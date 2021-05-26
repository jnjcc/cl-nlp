;;;; Copyright (c) 2012-2015 jnjcc, Yste.org. All rights reserved.
;;;;
;;;; Mutable Probability Distribution

(in-package #:cl-nlp)

(defclass dprob-mutable (freq-dist-hash)
  ())

(defmethod init-from-literal ((pmutable dprob-mutable) keylst problst)
  "`keylst': (key1 key2 ...); problst: (prob1 prob2 ...)"
  (dotimes (i (length keylst))
    (fdist-setf pmutable (nth i keylst) (nth i problst))))

(defmethod pdist-prob ((pmutable dprob-mutable) key)
  (fdist-value pmutable key))

(defmethod pdist-setf ((pmutable dprob-mutable) key value)
  (fdist-setf pmutable key value))

(defmethod pdist-incf ((pmutable dprob-mutable) key value)
  (fdist-incf pmutable key value))

(defmethod pdist-normalize ((pmutable dprob-mutable))
  (let ((sumprob (fdist-sum pmutable)))
    (labels ((normalize (key value)
               (fdist-setf pmutable key (/ value sumprob))))
      (for-each-pair pmutable #'normalize))))

(defclass dcond-mutable (dcond-prob-dist)
  ())

(defmethod pdist-cond-setf ((cpdist dcond-mutable) condi key value)
  (let ((pmutable (fdist-value cpdist condi)))
    (unless pmutable
      (setf pmutable (make-instance 'dprob-mutable)))
    (pdist-setf pmutable key value)))

(defmethod pdist-cond-normalize ((cpdist dcond-mutable))
  (labels ((normalize (condi pdist)
             (fdist-setf cpdist condi (pdist-normalize pdist))))
    (for-each-pair cpdist #'normalize)))
