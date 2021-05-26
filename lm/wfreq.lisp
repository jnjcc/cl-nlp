;;;; Copyright (c) 2012-2015 jnjcc, Yste.org. All rights reserved.
;;;;
;;;; word frequency as hash-table: word -> frequency

(in-package #:cl-nlp)

(defun incf-frequency (whash word &optional (delta 1))
  (multiple-value-bind (freq exist) (gethash word whash)
    (unless exist
      (setf freq 0))
    (incf freq delta)
    (setf (gethash word whash) freq)))

(defun word-frequency (stream &key (eow #\Space) (eol #\Newline))
  "hash-table: word -> frequency"
  (let ((whash (make-hash-table :test 'equal)))
    (do-word (word stream :eow eow :eol eol)
      (unless (eq word :eol)
        (incf-frequency whash word)))
    whash))

(defun sorted-words (whash &key (compare #'>) (filter :key) (minfreq 0))
  (let ((vocab-lst nil))
    (labels ((collect (word freq)
               (when (> freq minfreq)
                 (ecase filter
                   ((:full :key) (push (list word freq) vocab-lst))
                   (:value (push freq vocab-lst))))))
      (maphash #'collect whash))
    (ecase filter
      (:full (sort vocab-lst compare :key (lambda (info) (second info))))
      (:key (mapcar #'car (sort vocab-lst compare :key (lambda (info)
                                                         (second info)))))
      (:value (sort vocab-lst compare)))))
