;;;; Copyright (c) 2012-2015 jnjcc, Yste.org. All rights reserved.
;;;;

(in-package #:cl-nlp/test)

(define-test algo-dict-test
  (let* ((keys '("A" "B" "C"))
         (vals '( 1   2   3))
         (badict (zip-badict keys vals))
         (bhdict (zip-bhdict keys vals)))
    (assert-eq 1 (badict-ref badict "A"))
    (assert-eq 2 (badict-ref badict "B"))
    (assert-true (string= "C" (badict-bref badict 3)))
    (assert-eq nil (badict-ref badict "D"))
    (assert-eq nil (badict-bref badict 5))

    (badict-add badict "A" 10)
    (assert-eq 10 (badict-ref badict "A"))
    (assert-true (string= "A" (badict-bref badict 10)))

    (assert-eq 3 (bhdict-ref bhdict "C"))
    (assert-true (string= "A" (bhdict-bref bhdict 1)))
    (assert-true (string= "B" (bhdict-bref bhdict 2)))
    (assert-eq nil (bhdict-ref bhdict "D"))
    (assert-eq nil (bhdict-bref bhdict 5))

    (bhdict-add bhdict "B" 20)
    (assert-eq 20 (bhdict-ref bhdict "B"))
    (assert-true (string= "B" (bhdict-bref bhdict 20)))))
