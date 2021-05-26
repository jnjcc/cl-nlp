;;;; Copyright (c) 2012-2015 jnjcc, Yste.org. All rights reserved.
;;;;

(in-package #:cl-nlp/test)

(define-test hmm-tagger-test
  (if (probe-file *treebank-dir*)
      (let* ((treebank (cl-nlp::load-treebank-dir *treebank-dir*))
             (tagcorpus (cl-nlp::parse-treebank treebank))
             (trains (subseq tagcorpus 0 3000))
             (validation (subseq tagcorpus 0 100))
             (sent nil)
             (hmmtag (make-instance 'hmm-tagger)))
        (hmm-learn hmmtag trains)
        (assert-eq 46 (length (hmm-states hmmtag)))
        (assert-eq 10767 (length (hmm-symbols hmmtag)))
        (let ((*float-zero-epsilon* 0.0001))
          (assert-true (float= 0.1793 (hmm-start-prob hmmtag "NNP")))
          (assert-true (float= 0.0000 (hmm-start-prob hmmtag "WP$")))
          (assert-true (float= 0.0243 (hmm-start-prob hmmtag "-NONE-")))
          (assert-true (float= 0.3901 (hmm-trans-prob hmmtag "NNP" "NNP")))
          (assert-true (float= 0.0000 (hmm-trans-prob hmmtag "WP$" "NNP")))
          (assert-true (float= 0.0061 (hmm-trans-prob hmmtag "NNP" "-NONE-")))
          (assert-true (float= 0.0432 (hmm-emiss-prob hmmtag "NNP" "Mr.")))
          (assert-true (float= 0.9855 (hmm-emiss-prob hmmtag "." ".")))
          (assert-true (float= 0.1623 (hmm-emiss-prob hmmtag "-NONE-" "0"))))

        (setf sent (cl-nlp::untag-sentence (car trains)))
        (assert-true (equal (car trains) (hmm-decode hmmtag sent)))
        (assert-true (> (pos-evaluate hmmtag validation) 0.98))

        (hmm-learn hmmtag trains :lidstone)
        (let ((*float-zero-epsilon* 0.0001))
          ;; `SYM' only transition to `NNP' in train data, smooth with :lidstone
          (assert-true (float= 0.0178 (hmm-trans-prob hmmtag "SYM" "SYM"))))
        )
      (error "treebank corpus needed (https://github.com/nltk/nltk)")))
