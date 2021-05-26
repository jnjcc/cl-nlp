;;;; Copyright (c) 2012-2015 jnjcc, Yste.org. All rights reserved.
;;;;

(in-package #:cl-nlp/test)

(define-test treebank-test
  (if (probe-file *treebank-dir*)
      (let* ((treebank (cl-nlp::load-treebank-dir *treebank-dir*))
             (tagged (cl-nlp::parse-treebank treebank))
             (tagone nil))
        (assert-eq 3884 (length treebank))
        (assert-eq 3884 (length tagged))

        (setf tagone (nth 0 tagged))
        (assert-true (equal '("Pierre" . "NNP") (first tagone)))
        (assert-true (equal '("." . ".") (car (last tagone))))

        (setf tagone (nth 99 tagged))
        (assert-true (equal '("Northeast" . "NNP") (first tagone)))
        (assert-true (equal '("0" . "-NONE-") (third tagone)))
        (assert-true (equal '("." . ".") (car (last tagone)))))
      (error "treebank corpus needed (https://github.com/nltk/nltk)")))

(define-test conll-chunk-test
  (with-open-file (conll-fp (merge-pathnames "conll/head.txt" *dataset-path*))
    (let ((chunkt (read-file-content (merge-pathnames "conll/chunk.txt" *dataset-path*)))
          (postag (read-file-content (merge-pathnames "conll/pos.txt" *dataset-path*)))
          (output (make-adjustable-string)))
      (with-output-to-string (out-fp output)
        (cl-nlp::write-conll-chunk conll-fp out-fp))
      (assert-true (string= chunkt output))

      (setf output (make-adjustable-string))
      (with-output-to-string (out-fp output)
        (file-position conll-fp 0)
        (cl-nlp::write-conll-pos conll-fp out-fp))
      (assert-true (string= postag output)))))

(define-test crfsuite-test
  (with-open-file (pos-fp (merge-pathnames "conll/pos.txt" *dataset-path*))
    (let ((inst (cl-nlp::read-crf-instance pos-fp)))
      (assert-eq 37 (length inst))
      (setf inst (cl-nlp::read-crf-instance pos-fp))
      (assert-eq 23 (length inst)))))
