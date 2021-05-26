;;;; Copyright (c) 2012-2015 jnjcc, Yste.org. All rights reserved.
;;;;
;;;; word ngram count as hash-table: ngram -> count

(in-package #:cl-nlp)

(defun wngram-count (stream n &key (eow #\Space) (eol #\Newline) (bos "<BOS>") (eos "<EOS>"))
  "hash-table: ngram -> count"
  (let ((ngram-hash (make-hash-table :test 'equal))
        (ngram nil)
        (slens nil))
    (do-sentence (sentence stream :eow eow :eol eol)
      (setf slens (length sentence))
      (when bos
        (dotimes (i (- n 1))
          (when (= i slens)
            (return))
          (setf ngram
                (format nil "~A~A"
                        (repeat-string (format nil "~A~A" bos " ") (- n 1 i))
                        (join-strings (subseq sentence 0 (+ i 1)))))
          (incf-frequency ngram-hash ngram)))
      (dotimes (i (- slens n -1))
        (setf ngram (join-strings (subseq sentence i (+ i n))))
        (incf-frequency ngram-hash ngram))
      (when eos
        (when (>= slens (- n 1))
          (setf ngram (format nil "~A ~A"
                              (join-strings (subseq sentence (- slens n -1) slens))
                              eos))
          (incf-frequency ngram-hash ngram))))
    ngram-hash))
