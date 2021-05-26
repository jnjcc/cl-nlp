;;;; Copyright (c) 2012-2015 jnjcc, Yste.org. All rights reserved.
;;;;
;;;; ARPA file format: write n-gram Language Model

(in-package #:cl-nlp)

(defun %write-cond-prob (idminfo m curidxs endidxs id2word stream)
  (let ((mgfreq nil)
        (condpr 0.0)
        (alpha 0.0)
        (log10prob 0.0)
        (log10alpha 0.0))
    (do ((idx (mlist-ref curidxs m) (mlist-ref curidxs m)))
        ((> idx (mlist-ref endidxs m)))
      (setf mgfreq (idm-get-freq idminfo m idx))
      (setf mgfreq (idm-discount-freq idminfo m mgfreq))
      (setf condpr (/ mgfreq (idm-get-freq idminfo (- m 1) (mlist-ref curidxs (- m 1)))))
      (if (> condpr 0.0)
          (setf log10prob (log condpr 10.0))
          (setf log10prob *log-infinity*))
      (format stream "~,4f" log10prob)
      (loop for k from 1 to m do
           (format stream " ~A" (gethash (idm-get-wdid idminfo k (mlist-ref curidxs k)) id2word)))
      (when (< m (idm-get-n idminfo))
        (setf alpha (idm-get-alpha idminfo m (mlist-ref curidxs m)))
        (if (> alpha 0.0)
            (setf log10alpha (log alpha 10.0))
            (setf log10alpha *log-infinity*))
        (format stream " ~,4f" log10alpha))
      (format stream "~%")
      (mlist-incf curidxs m))))

(defun %write-mgram (idminfo m idmin idmax id2word stream)
  "Refer to `idm-backoff-gram'"
  (let ((cgm 1)
        (curidxs (mlist-init (+ m 1) :initial-element 1))
        (endidxs (mlist-init (+ m 1) :initial-element 0)))
    (loop for id from idmin to idmax do
         (mlist-setf curidxs 1 id)
         (when (> (idm-get-freq idminfo 1 id) 0)
           (setf cgm 2)
           (%fill-next-pointer curidxs cgm idminfo endidxs)
           (do ((cgm 2))
               ((<= cgm 1))
             (cond
               ((= cgm m) (progn
                            (%write-cond-prob idminfo cgm curidxs endidxs id2word stream)
                            (decf cgm)
                            (when (> cgm 1)
                              (mlist-incf curidxs cgm))))
               ((< cgm m) (if (<= (mlist-ref curidxs cgm) (mlist-ref endidxs cgm))
                              (progn
                                (incf cgm)
                                (%fill-next-pointer curidxs cgm idminfo endidxs))
                              (progn
                                (decf cgm)
                                (when (> cgm 1)
                                  (mlist-incf curidxs cgm)))))))))))

(defun write-arpa (idminfo idmin idmax id2word &optional (stream t))
  (format stream "\\data\\~%")
  (format stream "ngram 1=~A~%" (+ 1 idmax (- idmin)))
  (loop for m from 2 to (idm-get-n idminfo) do
       (format stream "ngram ~A=~A~%" m (idm-get-gnum idminfo m)))

  (let ((log10prob nil)
        (log10alpha nil))
    (format stream "~%\\1-grams:~%")
    (loop for id from idmin to idmax do
         (setf log10prob (/ (idm-get-logpr idminfo id) (log 10.0)))
         (when (<= (idm-get-uprob idminfo id) 0.0)
           (setf log10prob *log-infinity*))
         (setf log10alpha *log-infinity*)
         (when (> (idm-get-alpha idminfo 1 id) 0.0)
           (setf log10alpha (/ (log (idm-get-alpha idminfo 1 id)) (log 10.0))))
         (if (> (idm-get-n idminfo) 1)
             (format stream "~,4f ~A ~,4f~%" log10prob (gethash id id2word) log10alpha)
             (format stream "~,4f ~A~%" log10prob (gethash id id2word)))))

  (loop for m from 2 to (idm-get-n idminfo) do
       (format stream "~%\\~A-grams:~%" m)
       (%write-mgram idminfo m idmin idmax id2word stream))

  (format stream "~%\\end\\~%"))
