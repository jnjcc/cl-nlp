;;;; Copyright (c) 2012-2015 jnjcc, Yste.org. All rights reserved.
;;;;

(in-package #:cl-nlp)

(defun ngram-lm (corpus-fp n lm-fp
                 &key (disc-type :good-turing) (disc-range *fof-disc-range*) (uni-range *fof-uni-range*)
                   (bos "-1") (eos "-2") (oov :open) (unk "<UNK>") (lmtype :arpa))
  (let* ((old-pos (file-position corpus-fp))
         (wfreq (word-frequency corpus-fp))
         (idvocab (wfreq-idvocab wfreq))
         (idngram nil))
    (file-position corpus-fp old-pos)
    (setf idngram (idngram-count corpus-fp n idvocab :bos bos :eos eos))
    (multiple-value-bind (idminfo id2word idmin idmax)
        (idngram-lm idngram n idvocab
                    :disc-type disc-type :disc-range disc-range :uni-range uni-range
                    :oov oov :unk unk :bos bos :eos eos)
      (ecase lmtype
        (:arpa (write-arpa idminfo idmin idmax id2word lm-fp))))))
