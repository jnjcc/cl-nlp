;;;; Copyright (c) 2012-2015 jnjcc, Yste.org. All rights reserved.
;;;;

(in-package #:cl-nlp/test)

(define-test w2v-vocab-test
  "Huffman tree of the form:
            8
           /  \
         6      7
        / \    / \
       5   B  D   A
      / \  2  3   4
     E   C
     0   1
where inner node being index, which should subtract length(leafs)"
  (let ((text "a a b c c b b a d e d d a a d")
        (vocab nil)
        (vocsz nil)
        (winfo nil))
    (setf vocab (cl-nlp::learn-vocab-from-string text))
    (cl-nlp::create-huffman vocab)
    (setf vocsz (cl-nlp::vocab-size vocab))

    (assert-eq 5 vocsz)
    (setf winfo (cl-nlp::get-word-info vocab "e"))
    ;; list of index of parents
    ;; (assert-true (equal '(8 6 5) (cl-nlp::winfo-path winfo)))
    (assert-true (equal '(3 1 0) (cl-nlp::winfo-path winfo)))
    ;; left child
    (assert-true (equal 0 (cl-nlp::winfo-code winfo)))
    (setf winfo (cl-nlp::get-word-info vocab "b"))
    ;; (assert-true (equal '(8 6) (cl-nlp::winfo-path winfo)))
    (assert-true (equal '(3 1) (cl-nlp::winfo-path winfo)))
    (assert-true (equal 1 (cl-nlp::winfo-code winfo)))))

(define-test word2vec-test
  (let ((text "a a b c c b b a d e d d a a d")
        ;; CBOW with Hierarchical Softmax
        (cbowhs (make-instance 'word2vec :dimensions 10 :epochs 50 :algo :cbow :hsoftmax t :window 3))
        ;; CBOW with Negative Sampling
        (cbowneg (make-instance 'word2vec :dimensions 10 :epochs 50 :algo :cbow :negsample 3 :window 3))
        ;; Skip-gram with Hierarchical Softmax
        (sghs (make-instance 'word2vec :dimensions 10 :epochs 50 :algo :skip-gram :hsoftmax t :window 3))
        ;; Skip-gram with Negative Sampling
        (sgneg (make-instance 'word2vec :dimensions 10 :epochs 50 :algo :skip-gram :negsample 3 :window 3)))
    (fit-from-string cbowhs text)
    ;; TODO: (assert-true (equal))
    (format *test-stream* "cbowhs a: ~A~%" (transform cbowhs "a"))
    (fit-from-string cbowneg text)
    (format *test-stream* "cbowneg a: ~A~%" (transform cbowneg "a"))
    (fit-from-string sghs text)
    (format *test-stream* "a: ~A~%" (transform sghs "a"))
    (fit-from-string sgneg text)
    (format *test-stream* "a: ~A~%" (transform sgneg "a"))))
