;;;; Copyright (c) 2012-2015 jnjcc, Yste.org. All rights reserved.
;;;;

(in-package #:cl-nlp/test)

(define-test hmm-model-test
  (let ((states '("bull" "bear" "static"))
        (symbls '("up" "down" "unchanged"))
        (startp '(0.5 0.2 0.3))
        (transp '((0.6 0.2 0.2) (0.5 0.3 0.2) (0.4 0.1 0.5)))
        (emissp '((0.7 0.1 0.2) (0.1 0.6 0.3) (0.3 0.3 0.4)))
        (hmm (make-instance 'hmm-model)))
    (cl-nlp::hmm-literal hmm states symbls startp transp emissp)
    (let ((obscorpus '(("up" "down" "up")
                       ("unchanged" "unchanged" "unchanged" "unchanged" "unchanged" "up")))
          (probs '(0.0509 0.0008))
          (statcorpus '(("bull" "bear" "bull")
                        ("static" "static" "static" "static" "static" "bull")))
          (*float-zero-epsilon* 0.0001))
      (dotimes (i (length obscorpus))
        (assert-true (float= (nth i probs) (hmm-forward hmm (nth i obscorpus))))
        (assert-true (float= (nth i probs) (hmm-backward hmm (nth i obscorpus))))
        (assert-true (equal (nth i statcorpus)
                            (mapcar #'cdr (hmm-decode hmm (nth i obscorpus))))))
      )))
