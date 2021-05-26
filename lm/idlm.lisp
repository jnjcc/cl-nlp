;;;; Copyright (c) 2012-2015 jnjcc, Yste.org. All rights reserved.
;;;;
;;;; idngram language model:
;;;;   corpus -> (word-frequency) -> (wfreq-idvocab) \
;;;;        \                               /         (idngram-lm)
;;;;         `---------------------> (idngram-count) /

(in-package #:cl-nlp)

(defun idngram-lm (idngram n idvocab
                   &key (disc-type :good-turing) (disc-range *fof-disc-range*) (uni-range *fof-uni-range*)
                     (bos "-1") (eos "-2") (oov :open) (unk "<UNK>"))
  "`disc': discount range for 2-gram, 3-gram, etc; `uni': discount range for 1-gram"
  (let ((id2word (make-hash-table :test 'equal))
        (idmin 0)
        (idmax -1))
    (maphash (lambda (word id) (setf (gethash id id2word) word)) idvocab)
    (setf idmax (hash-table-count id2word))
    (setf (gethash 0 id2word) unk)
    (when (eq oov :closed)
      ;; <UNK> not allowed
      (setf idmin 1))
    (let ((idminfo (idngram-tree idngram n idmax :disc disc-range :uni uni-range :bos bos :eos eos)))
      ;; for unigram, there are frequency zero's: e.g., <UNK>'s frequency will definitely be zero
      (loop for id from idmin to idmax do
           (when (= (idm-get-freq idminfo 1 id) 0)
             (idm-incf-fof idminfo 1 0)))
      (format nil "fof: ~A~%" (mgram-fof idminfo))
      (idm-discount-ratio idminfo disc-type)
      (format nil "disc-ratio: ~A~%" (mgram-disc idminfo))
      (idm-discount-ugram idminfo idmin idmax 1.0)
      (loop for m from 2 to n do
           (idm-backoff-gram idminfo m idmin idmax))
      (format nil "probs: ~A~%" (ugram-prob idminfo))
      (values idminfo id2word idmin idmax))))
