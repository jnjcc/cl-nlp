;;;; Copyright (c) 2012-2015 jnjcc, Yste.org. All rights reserved.
;;;;
;;;; Probability Distribution API:
;;;;
;;;;           freq-dist-bins
;;;;          /
;;;; freq-dist                cond-freq-dist -> dcond-prob-dist -> dcond-mutable
;;;;          \              /
;;;;           freq-dist-hash
;;;;             ^           \
;;;;             |            dprob-mutable
;;;;             |                       mle-prob-dist
;;;;             |                      /
;;;;        (view window) <-  dprob-dist
;;;;                                    \
;;;;                                     lidstone-prob-dist

(in-package #:cl-nlp)

;;; make-* functions from frequency distribution
(defun derive-prob (fdist bins type &key (gamma 1.0))
  "derive probability from `fdist' using `type' estimator"
  (let ((pdist nil))
    (ecase type
      (:mle (setf pdist (make-instance 'mle-prob-dist :bins bins)))
      (:lidstone (setf pdist (make-instance 'lidstone-prob-dist :bins bins :gamma gamma))))
    (estimate-prob pdist fdist)
    pdist))

(defun derive-cond-prob (cfdist bins type &key (gamma 0.1))
  "derive conditional probability from `cfdist' using `type' estimator"
  (let ((dcondp (make-instance 'dcond-prob-dist :bins bins)))
    (with-slots (test) cfdist
      (with-slots (htable) dcondp
        (setf htable (make-hash-table :test test))
        (labels ((collect (condi fdist)
                   (setf (gethash condi htable) (derive-prob fdist bins type :gamma gamma))
                   ))
          (for-each-pair cfdist #'collect))))
    dcondp))

;;; make-* functions from probability literals
(defun derive-literal (keylst problst)
  (let ((pdist (make-instance 'dprob-mutable)))
    (init-from-literal pdist keylst problst)
    pdist))

(defun derive-cond-literal (conds keys probmat)
  "`probmat': list of list, prob from `cond' to `key'"
  (let ((cpmutable (make-instance 'dcond-mutable))
        (condi nil))
    (with-slots (htable) cpmutable
      (setf htable (make-hash-table :test 'equal))
      (dotimes (i (length conds))
        (setf condi (nth i conds))
        (setf (gethash condi htable) (derive-literal keys (nth i probmat)))))
    cpmutable))

;;; copy-* from derived probability to mutable probability
(defun copy-mutable-dprob (pdist &key (fill-value nil))
  (let ((pmutable (make-instance 'dprob-mutable)))
    (dolist (key (pdist-keys pdist))
      (if fill-value
          (pdist-setf pmutable key fill-value)
          (pdist-setf pmutable key (pdist-prob pdist key))))
    pmutable))

(defun copy-mutable-dcond (cpdist &key (fill-value nil))
  (let ((cpmutable (make-instance 'dcond-mutable)))
    (with-slots (htable) cpmutable
      (setf htable (make-hash-table :test 'equal))
      (dolist (condi (pdist-cond-conds cpdist))
        (setf (gethash condi htable) (copy-mutable-dprob (fdist-value cpdist condi)
                                                         :fill-value fill-value))))
    cpmutable))

;;; log probability
(defvar *log-infinity* -99.999)

(defun logprob (prob)
  (if (> prob 0.0)
      (log prob)
      *log-infinity*))

(defun logprod (&rest probs)
  "log(prod1 * prod2 * ...) = log(prod1) + log(prod2) + ..."
  (reduce #'+ (mapcar #'logprob probs) :initial-value 0.0))

(defun rlogprod (&rest probs)
  "prob1 + log(prob2) + ..., i.e., prob1 already in log form"
  (+ (car probs) (apply #'logprod (cdr probs))))

(defun sumexp (&rest logprobs)
  "meant to calculate probability summation
NOTICE: unlike (cl-ml::logsumexp), there will be no overflow problems for exp()
with probability as operands"
  (reduce #'+ (mapcar #'exp logprobs) :initial-value 0.0))

(defun logsumexp (&rest logprobs)
  "probability summation and then do log again
NOTICE: unlike (cl-ml::logsumexp), there will be no overflow problems for exp()
with probability operands"
  (logprob (apply #'sumexp logprobs)))

;;; random probability
(defun randprob1d (n &optional (state *random-state*))
  (let* ((prob1d (randuni 1.0 n state))
         (sum (reduce #'+ prob1d :initial-value 0.0)))
    (mapcar (lambda (x) (/ x sum)) prob1d)))

(defun randprob2d (m n &optional (state *random-state*))
  (let ((prob2d (make-list m :initial-element nil)))
    (dotimes (i m)
      (setf (nth i prob2d) (randprob1d n state)))
    prob2d))

