;;;; Copyright (c) 2012-2015 jnjcc, Yste.org. All rights reserved.
;;;;
;;;; idngram count as hash-table: idngram -> count

(in-package #:cl-nlp)

(defun %join-idngram (sentence beg end idvocab)
  (join-strings (mapcar (lambda (word) (gethash word idvocab))
                        (subseq sentence beg end))))

(defun idngram-count (stream n idvocab &key (eow #\Space) (eol #\Newline) (bos -1) (eos -2))
  "hash-table: idngram -> count"
  (let ((idngram-hash (make-hash-table :test 'equal))
        (idngram nil)
        (slens nil))
    (do-sentence (sentence stream :eow eow :eol eol)
      (setf slens (length sentence))
      (when bos
        (dotimes (i (- n 1))
          (when (= i slens)
            (return))
          (setf idngram
                (format nil "~A~A"
                        (repeat-string (format nil "~A~A" bos " ") (- n 1 i))
                        (%join-idngram sentence 0 (+ i 1) idvocab)))
          (incf-frequency idngram-hash idngram)))
      (dotimes (i (- slens n -1))
        (setf idngram (%join-idngram sentence i (+ i n) idvocab))
        (incf-frequency idngram-hash idngram))
      (when eos
        (when (>= slens (- n 1))
          (setf idngram (format nil "~A ~A"
                                (%join-idngram sentence (- slens n -1) slens idvocab)
                                eos))
          (incf-frequency idngram-hash idngram))))
    idngram-hash))

(defun wngram-idngram (wngram idvocab &key (wbos "<BOS>") (bos -1) (weos "<EOS>") (eos -2))
  "word ngram -> id ngram"
  (let ((idngram-hash (make-hash-table :test 'equal)))
    (labels ((translate (ngram freq)
               (setf ngram (join-strings (mapcar (lambda (word)
                                                   (cond
                                                     ((string= word wbos) bos)
                                                     ((string= word weos) eos)
                                                     (t (gethash word idvocab))))
                                                 (split-string ngram #\Space))))
               (incf-frequency idngram-hash ngram freq)))
      (maphash #'translate wngram))
    idngram-hash))
