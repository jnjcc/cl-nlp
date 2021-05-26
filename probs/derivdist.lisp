;;;; Copyright (c) 2012-2015 jnjcc, Yste.org. All rights reserved.
;;;;
;;;; Derived Probability Distribution from Frequency Distribution

(in-package #:cl-nlp)

;;; Derive (or, estimate) probability distribution from Frequency Distribution `fdist'
;;;   just a view window of `fdist'
(defclass dprob-dist ()
  ((fdist :initform nil)
   ;; how many keys are there in `fdist'?
   (bins :initform nil :initarg :bins)))

(defgeneric estimate-prob (pdist fdist)
  (:documentation "estimate probability from frequency distribution `fdist'"))

(defmethod pdist-keys ((pdist dprob-dist))
  (with-slots (fdist) pdist
    (fdist-keys fdist)))

(defgeneric pdist-prob (pdist key)
  (:documentation "P(key)"))

;;; Derive (or, estimate) conditional probability distribution
(defclass dcond-prob-dist (cond-freq-dist)
  ((bins :initform nil :initarg :bins)))

(defmethod pdist-cond-conds ((cpdist dcond-prob-dist))
  (fdist-keys cpdist))

(defmethod pdist-cond-prob ((cpdist dcond-prob-dist) condi key)
  "P(key | condi)"
  (let ((pdist (fdist-value cpdist condi)))
    (if pdist
        (pdist-prob pdist key)
        0.0)))

;;; Maximum Likelihood Estimate from Frequency Distribution
(defclass mle-prob-dist (dprob-dist)
  ())

(defmethod estimate-prob ((mle mle-prob-dist) from-fdist)
  (with-slots (fdist) mle
    (setf fdist from-fdist)))

(defmethod pdist-prob ((mle mle-prob-dist) key)
  (with-slots (fdist) mle
    (/ (fdist-freq fdist key) (fdist-sum fdist))))

;;; Lidstone Estimate from Frequency Distribution
;;;   (c + gamma) / (N + B * gamma)
(defclass lidstone-prob-dist (dprob-dist)
  ((gamma :initform 1.0 :initarg :gamma)))

(defmethod estimate-prob ((lidstone lidstone-prob-dist) from)
  (with-slots (fdist) lidstone
    (setf fdist from)))

(defmethod pdist-prob ((lidstone lidstone-prob-dist) key)
  (with-slots (fdist bins gamma) lidstone
    (let ((realbins bins))
      ;; NOTICE: if `bins' supplied, use it! (some bin with freq=0 might not be in `fdist')
      ;;   otherwise, derive bins from `fdist'
      (unless realbins
        (setf realbins (fdist-bin fdist)))
      (/ (+ (fdist-freq fdist key) gamma)
         (+ (fdist-sum fdist) (* realbins gamma))))))
