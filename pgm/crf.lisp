;;;; Copyright (c) 2012-2015 jnjcc, Yste.org. All rights reserved.
;;;;
;;;; (First-order, linear-chain) Conditional Random Fields

(in-package #:cl-nlp)

;;; using CRFsuite jargons:
;;;   - `attrsd': attribute name bidictionary
;;;   - `labeld': label name bidictionary
(defclass crf-model ()
  ((attrsd :initform nil :documentation "attribute name dictionary: attr-name <-> index")
   (labeld :initform nil :documentation "label name dictionary:     labl-name <-> index")
   (statef :initform nil :documentation "state feature f(s, x, t) as a 2D array-like [s][aid],
like Unigram features in CRF++, which generates |Y| x |U| features")
   (transf :initform nil :documentation "transition feature f(s', s, x, t) as a 2D array-like [s'][s],
NOTICE: should have been a 3D array-like [s'][s][aid], like Bigram features in CRF++,
which generates |Y| x |Y| x |B| features")
   (sweigt :initform nil :documentation "CRF model weights for state features")
   (tweigt :initform nil :documentation "CRF model weights for transition features")))

(defmethod generate-features ((crf crf-model) crfdata)
  (with-slots (transf statef tweigt sweigt) crf
    (setf transf (make-cond-freq))
    (setf statef (make-cond-freq))
    (let ((plid nil))
      (do-crf-instance (inst crfdata)
        (setf plid nil)
        (do-crf-item (lid attrs inst)
          (when plid
            ;; transition feature: <(s', s), 1>
            (fdist-cond-incf transf plid lid 1))
          (do-crf-attr (aid scale attrs)
            ;; state feature: <(s, aid), scale>
            (fdist-cond-incf statef lid aid scale))
          (setf plid lid))))
    (setf sweigt (copy-mutable-dcond statef :fill-value 0))
    (setf tweigt (copy-mutable-dcond transf :fill-value 0))
    (do-cond-pairs (lid aid scale statef)
      (format t "~A -> ~A: ~A~%" (crfdata-get-labl crfdata lid) (crfdata-get-aname crfdata aid)
              scale))
    (do-cond-pairs (plid lid val transf)
      (format t "~A -> ~A: ~A~%" (crfdata-get-labl crfdata plid) (crfdata-get-labl crfdata lid)
              val))
    ))

(defmethod crf-potential-one ((crf crf-model) item)
  (let ((ptval 0))
    ;; (car item) being a list, holding label, and possibly word itself
    ;;   e.g. ("NN" "Confidence")
    ptval))

(defmethod crf-potential ((crf crf-model) inst)
  (with-slots (states weight) crf
    (let* ((inT (length inst))
           (ptt-list nil))
      )))

(defmethod crf-forward-alpha ((crf crf-model) inst)
  (with-slots (states weight) crf
    (let* ((inT (length inst))
           (stN (length states))
           (alpha (make-array (list (+ inT 2) stN) :initial-element 0)))
      alpha)))

(defmethod crf-backward-beta ((crf crf-model) inst)
  (with-slots (states weight) crf
    ))

(defmethod crf-learn ((crf crf-model) crfdata &optional (algo :sgd))
  (etypecase algo
    (:sgd (crf-learn-sgd crf crfdata))))
