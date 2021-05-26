;;;; Copyright (c) 2012-2015 jnjcc, Yste.org. All rights reserved.
;;;;

(in-package #:cl-nlp)

(defun %get-next-pointer (curidxs m idminfo)
  "given `curidxs[m]' as the current m-gram, what is the latest (m+1)-gram it points to?"
  (if (= (mlist-ref curidxs m) (idm-get-gnum idminfo m))
      ;; points to the last (m+1)-gram
      (idm-get-gnum idminfo (+ m 1))
      (idm-get-ptrs idminfo m (mlist-ref curidxs m))))

(defun %fill-next-pointer (curidxs m idminfo endidxs &key (verbose nil))
  "given `curidxs[m-1]' as the current (m-1)-gram, fill `endidxs[m]'"
  (mlist-setf endidxs m (%get-next-pointer curidxs (- m 1) idminfo))
  (when verbose
    (format t "~A-th ~A-gram points to ~A-th ~A-gram~%"
            (mlist-ref curidxs (- m 1)) (- m 1)
            (mlist-ref endidxs m) m)))

(defun %sum-cond-probs (curidxs endidxs m idminfo)
  "current (m-1)-gram `curidxs[m-1]' spans from `curidxs[m]' to `endidxs[m]'-th m-gram,
sum of conditional probability might not be 1.0, as frequency might be discounted"
  (let ((mgfreq nil) ;; current m-gram frequency (might be discounted)
        (condpr 0.0) ;; conditional probability: P(C|AB)
        (sumcpr 0.0) ;; sum of conditional probability
        (katzpr 0.0)
        (sumkatz 0.0))
    (do ((idx (mlist-ref curidxs m) (mlist-ref curidxs m)))
        ((> idx (mlist-ref endidxs m)))
      (setf mgfreq (idm-get-freq idminfo m idx))
      (setf mgfreq (idm-discount-freq idminfo m mgfreq))
      ;; P(C|AB) = freq(ABC) / freq(AB)
      (setf condpr (/ mgfreq (idm-get-freq idminfo (- m 1) (mlist-ref curidxs (- m 1)))))
      (incf sumcpr condpr)
      (let ((idlist nil))
        ;; backoff: from (w1, w2, w3) to (w2, w3)
        (loop for k from 2 to m do
             (push (idm-get-wdid idminfo k (mlist-ref curidxs k)) idlist))
        (setf katzpr (katz-backoff (nreverse idlist) (- m 2) idminfo))
        (incf sumkatz katzpr))
      (mlist-incf curidxs m))
    (values sumcpr sumkatz)))

(defmethod idm-backoff-gram ((idminfo idinfo-type) n idmin idmax)
  "backoff alpha for n-gram (n >= 2)"
  (with-slots (mgram-alphas) idminfo
    (let ((cgm 1) ;; current gram m
          ;; `curidxs'-th m-gram, only unigram needs the fake 0-th gram
          (curidxs (mlist-init (+ n 1) :initial-element 1))
          ;; `curidxs'-th m-gram points to `endidxs'-th (m+1)-gram
          (endidxs (mlist-init (+ n 1) :initial-element 0)))
      (loop for id from idmin to idmax do
           (mlist-setf curidxs 1 id) ;; for each unigram, possibly starting from 0
           (cond
             ((= (idm-get-freq idminfo 1 id) 0)
              ;; deal with zeroton unigrams
              (when (= n 2)
                (idm-setf-alpha idminfo 1 (mlist-ref curidxs 1) 1.0)))
             ((> (idm-get-freq idminfo 1 id) 0)
              (progn
                (setf cgm 2)
                (%fill-next-pointer curidxs cgm idminfo endidxs)
                (do ((cgm 2))
                    ((<= cgm 1))
                  (cond
                    ((= cgm n) (multiple-value-bind (sumcpr sumkatz)
                                   (%sum-cond-probs curidxs endidxs cgm idminfo)
                                 (let ((leftmass (- 1.0 sumcpr))
                                       (leftkatz (- 1.0 sumkatz)))
                                   (if (<= leftkatz 1e-10)
                                       (idm-setf-alpha idminfo (- n 1) (mlist-ref curidxs (- n 1)) 0.0)
                                       (idm-setf-alpha idminfo (- n 1) (mlist-ref curidxs (- n 1))
                                                       (/ leftmass leftkatz))))
                                 ;; current (m-1)-gram finished, explore next (m-1)-gram
                                 (decf cgm)
                                 (when (> cgm 1)
                                   (mlist-incf curidxs cgm))))
                    ;; `cgm' < n: (m+1)-gram might be available to explore
                    ((< cgm n) (if (<= (mlist-ref curidxs cgm) (mlist-ref endidxs cgm))
                                   (progn
                                     (incf cgm)
                                     (%fill-next-pointer curidxs cgm idminfo endidxs))
                                   (progn
                                     (decf cgm)
                                     (when (> cgm 1)
                                       (mlist-incf curidxs cgm)))))))))))
      )))
