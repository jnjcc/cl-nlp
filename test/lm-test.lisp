;;;; Copyright (c) 2012-2015 jnjcc, Yste.org. All rights reserved.
;;;;

(in-package #:cl-nlp/test)

(define-test ngram-count-test
  (let ((text (format nil "~A~%~A~%~A~%~A" "One Ring to rule them all"
                      "One Ring to find them" "One Ring to bring them all"
                      "and in the darkness bind them"))
        wfreq wngram wmgram sorted
        idvocab idngram idngram2)
    (with-input-from-string (stream text)
      (setf wfreq (word-frequency stream))
      (file-position stream 0)
      (setf wngram (wngram-count stream 2))
      (assert-eq 3 (gethash "Ring" wfreq))
      (assert-eq 2 (gethash "all" wfreq))
      (assert-eq 1 (gethash "darkness" wfreq))
      (assert-eq 3 (gethash "<BOS> One" wngram))
      (assert-eq 2 (gethash "them <EOS>" wngram))
      (assert-eq 1 (gethash "to rule" wngram))

      (setf wmgram (ngram-mgram wngram 2 1))
      (assert-true 3 (gethash "Ring" wmgram))
      (assert-true 2 (gethash "all" wmgram))

      (setf sorted (sorted-wngram wngram 2 1))
      (assert-true (string= "Ring 3" (nth 1 sorted)))
      (assert-true (string= "all 2" (nth 2 sorted)))

      (setf idvocab (wfreq-idvocab wfreq))
      (file-position stream 0)
      (setf idngram (idngram-count stream 2 idvocab))
      ;; <BOS> One
      (assert-eq 3 (gethash "-1 1" idngram))
      ;; them <EOS>
      (assert-eq 2 (gethash "3 -2" idngram))

      (setf idngram2 (wngram-idngram wngram idvocab))
      ;; <BOS> One
      (assert-eq 3 (gethash "-1 1" idngram2))
      (assert-eq 2 (gethash "3 -2" idngram2))
      )))

(define-test frequency-of-frequency-test
  (let ((text (format nil "One Ring to rule them all them all"))
        (bos "<BOS>") (eos "<EOS>")
        wngram fof)
    (with-input-from-string (stream text)
      ;;; trigram:
      ;; <One Ring to 1>, <Ring to rule 1>, <to rule them 1>, <rule them all 1>, <them all them 1>, <all them all 1>,
      ;; <BOS BOS One 1>, <BOS One Ring 1>, <them all EOS 1>
      ;;; biggram:
      ;; <One Ring 1>, <Ring to 1>, <to rule 1>, <rule them 1>, <all them 1>, <them all 2>
      ;; <BOS One 1>, <all EOS 1>
      ;;; unigram:
      ;; <One 1>, <Ring 1>, <to 1>, <rule 1>, <them 2>, <all 2>
      (setf wngram (wngram-count stream 3 :bos bos :eos eos))
      (setf fof (fof-from-ngram wngram 3 :disc 7 :uni 3 :bos bos :eos eos))
      ;; bigram
      (assert-eq 7 (fof-ref fof 2 1))
      (assert-eq 1 (fof-ref fof 2 2))
      ;; we also recorded frequency of frequency-(disc+1)
      (assert-eq 0 (fof-ref fof 2 8))
      ;; unigram
      (assert-eq 4 (fof-ref fof 1 1))
      (assert-eq 2 (fof-ref fof 1 2))

      (file-position stream 0)
      (setf wngram (wngram-count stream 3 :bos nil :eos nil))
      (setf fof (fof-from-ngram wngram 3 :disc 7 :uni 3))
      ;; unigram
      (assert-eq 6 (fof-ref fof 1 1))
      ;; NOTICE: without `eos', the last two word `them' and `all' are lost...
      (assert-eq 0 (fof-ref fof 1 2)))))

(define-test idngram-tree-test
  ;; idngram contents:
  ;;   1 1 2 1
  ;;   1 2 1 1
  ;;   1 2 3 1
  ;;   2 1 1 1
  ;;   2 3 4 1
  ;;   3 4 5 1
  ;;   4 5 6 1
  ;;   5 6 7 1
  (with-open-file (idfp (merge-pathnames "lm/idngram.tree" *dataset-path*))
    (let* ((idmax 7) ;; 7 words in idngram file
           (idinfo (cl-nlp::idngram-tree (read-ngram idfp) 3 idmax :disc 7 :uni 3)))
      (assert-eq 3 (cl-nlp::idm-get-fof idinfo 1 1))
      ;; NOTICE: there should have been 8 bigrams, without <EOS> extending, we lost "6 7"
      (assert-true (equalp '(0 7 7 8) (cl-nlp::mgram-nums idinfo)))
      (assert-eq 2 (cl-nlp::idm-get-freq idinfo 2 2))
      (assert-eq 3 (cl-nlp::idm-get-wdid idinfo 2 4))
      (assert-eq 1.0 (cl-nlp::idm-get-alpha idinfo 2 2))

      ;; the 1-th unigram ("1") points to the 2-th bigram ("1 2")
      ;;   thus "1" spans 1-th to 2-th bigram ("1 1" and "1 2")
      (assert-eq 2 (cl-nlp::idm-get-ptrs idinfo 1 1))
      ;; the 2-th unigram ("2") points to the 4-th bigram ("2 3")
      ;;   thus "2" spans 3-th to 4-th bigram ("2 1" and "2 3")
      (assert-eq 4 (cl-nlp::idm-get-ptrs idinfo 1 2))
      ;; the 5-th unigram ("5") points to the 7-th bigram ("5 6")
      ;;   as well as the 6-th and 7-th unigram
      (assert-eq 7 (cl-nlp::idm-get-ptrs idinfo 1 5))
      (assert-eq 7 (cl-nlp::idm-get-ptrs idinfo 1 6))
      (assert-eq 7 (cl-nlp::idm-get-ptrs idinfo 1 7))

      ;; the 1-th bigram ("1 1") points to the 1-th trigram ("1 1 2")
      (assert-eq 1 (cl-nlp::idm-get-ptrs idinfo 2 1))
      ;; the 2-th bigram ("1 2") points to the 3-th trigram ("1 2 3")
      ;;   thus "1 2" spans 2-th to 3-th trigram ("1 2 1" and "1 2 3")
      (assert-eq 3 (cl-nlp::idm-get-ptrs idinfo 2 2)))))

(define-test idngram-lm-test
  (with-open-file (fcorpus (merge-pathnames "lm/corpus.lm" *dataset-path*))
    (let ((n 3)
          (disc-type :good-turing)
          (disc-range 2)
          (uni-range 1)
          (oov :open)
          (unk "<UNK>")
          wfreq idvocab idngram)
      (setf wfreq (word-frequency fcorpus))
      (setf idvocab (wfreq-idvocab wfreq))
      (file-position fcorpus 0)
      (setf idngram (idngram-count fcorpus n idvocab :bos nil :eos nil))
      (multiple-value-bind (idminfo id2word idmin idmax)
          (cl-nlp::idngram-lm idngram n idvocab
                              :disc-type disc-type :disc-range disc-range :uni-range uni-range
                              :oov oov :unk unk :bos nil :eos nil)
        (declare (ignore id2word idmin idmax))
        (let ((*float-zero-epsilon* 1.0e-4))
          (assert-true (float= 0.0134 (cl-nlp::idm-get-uprob idminfo 1)))
          (assert-true (float= 0.0278 (cl-nlp::idm-get-uprob idminfo 5)))
          (assert-true (float= 0.0556 (cl-nlp::idm-get-uprob idminfo 11)))
          (assert-true (float= 0.1250 (cl-nlp::idm-get-uprob idminfo 36)))
          )
        ))))

(define-test ngram-lm-test
  (with-open-file (corpus-fp (merge-pathnames "lm/corpus.lm" *dataset-path*))
    (let ((n 3)
          (disc-range 2)
          (uni-range 1)
          (lmarpa (read-file-content (merge-pathnames "lm/arpa.lm" *dataset-path*)))
          (lmstring (make-array '(0) :element-type 'base-char :fill-pointer 0 :adjustable t)))
      (with-output-to-string (lm-fp lmstring)
        (ngram-lm corpus-fp n lm-fp :disc-range disc-range :uni-range uni-range
                  :bos nil :eos nil))
      (assert-true (string= lmarpa lmstring)))))
