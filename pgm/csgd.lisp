;;;; Copyright (c) 2012-2015 jnjcc, Yste.org. All rights reserved.
;;;;
;;;; SGD for CRF

(in-package #:cl-nlp)

(defmethod crf-learn-sgd ((crf crf-model) crfdata)
  )

;;; Given `inst', expectation of feature function under empirical distribution
;;;   w[k] <- w[k] + eta * \sum_{t=1}^{T}f[k](y', y, x, t)
;;; or in vector form:
;;;   w <- w + eta * F(y, x)
(defmethod csgd-emp-exp ((crf crf-model) inst eta)
  (with-slots (statef transf sweigt tweigt) crf
    (let ((plid nil))
      (do-crf-item (lid attrs inst)
        ;; `lid' -> `aid': state feature
        (do-crf-attr (aid scale attrs)
          (fdist-cond-incf sweigt lid aid (* eta scale)))
        ;; `plid' -> `lid': transition feature
        (when plid
          (fdist-cond-incf tweigt plid lid eta))
        (setf plid lid)
        ))))

;;; Given `inst', expectation of feature function under model distribution
;;;   w <- w + eta * (-P(y|x)) * F(y, x)
(defmethod csgd-mod-exp ((crf crf-model) inst eta)
  )

(defmethod csgd-gradient ((crf crf-model) inst eta)
  )
