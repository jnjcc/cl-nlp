;;;; Copyright (c) 2012-2015 jnjcc, Yste.org. All rights reserved.
;;;;
;;;; sort n-gram (possibly reduce to m-gram and sort)

(in-package #:cl-nlp)

;;; wngram (as a list) sort (alphabetical order)
(defun %wngram< (wng1 wng2)
  (string< (join-strings wng1) (join-strings wng2)))

;;; idngram (as a list) sort (numeric order)
(defun %idngram< (idng1 idng2)
  ;; idng1 = (n-gram freq)
  (let ((n (- (length idng1) 1))
        (id1 nil)
        (id2 nil))
    (dotimes (i n)
      (setf id1 (parse-integer (nth i idng1) :junk-allowed t))
      (setf id2 (parse-integer (nth i idng2) :junk-allowed t))
      (cond
        ((< id1 id2) (return-from %idngram< t))
        ((> id1 id2) (return-from %idngram< nil))))
    (return-from %idngram< nil)))

;; idngram (as a string) sort
(defun %idnstr< (idnstr1 idnstr2)
  (let ((idng1 (split-string idnstr1 #\Space))
        (idng2 (split-string idnstr2 #\Space)))
    (%idngram< idng1 idng2)))

;;; now let's sort!
(defun %sorted-extend-2 (ngram &key (idngram-p nil))
  "for bigram, no need to extend!"
  (let ((sorted nil))
    (labels ((collect (gramkey freq)
               (push (cons (split-string gramkey #\Space) freq) sorted)))
      (maphash #'collect ngram))
    (if idngram-p
        (sort sorted #'%idngram< :key #'car)
        (sort sorted #'%wngram< :key #'car))))

(defun %sorted-extend-n (ngram n &key (eos "<EOS>") (idngram-p nil))
  "for each gram (A B C <eos>), extend (B C <eos>) and (C <eos> <eos>)
Requires: n > 2"
  (let ((sorted nil))
    (labels ((collect (gramkey freq)
               (let ((gramlst (split-string gramkey #\Space)))
                 (unless (= (length gramlst) n)
                   (error "gram [~A] not of size ~A" gramkey n))
                 (push (cons gramlst freq) sorted)
                 (when (string= (nth (- n 1) gramlst) eos)
                   (dotimes (i (- n 2))
                     (setf gramkey (append (subseq gramlst (+ i 1) n)
                                           (make-list (+ i 1) :initial-element eos)))
                     (push (cons gramkey freq) sorted))))))
      (maphash #'collect ngram))
    (if idngram-p
        (sort sorted #'%idngram< :key #'car)
        (sort sorted #'%wngram< :key #'car))))

(defun sorted-extend (ngram n &key (eos "<EOS>") (idngram-p nil))
  (if (or (<= n 2) (null eos))
      (%sorted-extend-2 ngram :idngram-p idngram-p)
      (%sorted-extend-n ngram n :eos eos :idngram-p idngram-p)))
