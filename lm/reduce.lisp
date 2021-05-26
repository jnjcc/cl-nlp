;;;; Copyright (c) 2012-2015 jnjcc, Yste.org. All rights reserved.
;;;;
;;;; reduce n-gram to m-gram

(in-package #:cl-nlp)

(defun read-ngram (stream &key (eow #\Space))
  (let ((ngram-hash (make-hash-table :test 'equal))
        (ngram nil))
    (do ((line (read-line stream nil :eof)
               (read-line stream nil :eof)))
        ((eq line :eof))
      (setf ngram (split-string line eow))
      (setf (gethash (join-strings (butlast ngram)) ngram-hash)
            (read-from-string (car (last ngram)))))
    ngram-hash))

(defun write-ngram (whash &optional (stream t))
  (maphash (lambda (key val) (format stream "~A ~A~%" key val)) whash))

(defun ngram-mgram (ngram n m &key (bos "<BOS>") (eos "<EOS>"))
  "reduce n-gram hash-table `ngram' to m-gram
NOTICE: if there are no `eos', the result m-gram will not be precise"
  (when (<= n m)
    (error "Requires: n > m"))
  (let ((mgram-hash (make-hash-table :test 'equal)))
    (labels ((translate (gramkey freq)
               (let ((gramlst (split-string gramkey #\Space)))
                 (unless (= (length gramlst) n)
                   (error "gram [~A] not of size ~A" gramkey n))
                 (cond
                   ((string= (nth 0 gramlst) bos)
                    ;; skip grams full of `bos'
                    (or (every (lambda (word) (string= bos word)) (subseq gramlst 0 m))
                        (incf-frequency mgram-hash (join-strings (subseq gramlst 0 m)) freq)))
                   ((string= (nth (- n 1) gramlst) eos)
                    ;; if there are no `eos', we will lose those m-gram starting from 1, 2, ...
                    (dotimes (i (- n m))
                      (unless (= i (- n 1))
                        (incf-frequency mgram-hash (join-strings (subseq gramlst i (+ i m))) freq))))
                   (t
                    ;; m-grams starting from 1, 2, etc will be updated in following n-grams
                    (incf-frequency mgram-hash (join-strings (subseq gramlst 0 m)) freq))))))
      (maphash #'translate ngram))
    mgram-hash))

(defun %malformed-gram (grams m bos eos)
  "are m-gram `grams'[0:m) malformed?"
  (or
   ;; full of `bos'
   (string= (nth (- m 1) grams) bos)
   ;; more than one `eos', fake grams from `sorted-extend'
   (and (>= m 2) (string= (nth (- m 2) grams) eos))))

(defun %sorted-reduce-ngram (ngram n m bos eos &key (idngram-p nil))
  "reduce `ngram' of size n to size m, and collect into a list"
  (let* ((gramlst nil)
         (sorted (sorted-extend ngram n :eos eos :idngram-p idngram-p))
         (pre-gram (caar sorted))
         (pre-freq (cdar sorted))
         (running-pos 0)
         (running-total (cdar sorted))
         (cur-gram nil)
         (cur-freq nil)
         (diff-pos 0))
    (dotimes (i (- (length sorted) 1))
      (setf cur-gram (car (nth (+ i 1) sorted))
            cur-freq (cdr (nth (+ i 1) sorted)))
      (setf diff-pos n)
      (dotimes (j n)
        (when (string/= (nth j cur-gram) (nth j pre-gram))
          (setf diff-pos j)
          (return)))
      (when (= diff-pos n)
        (error "same gram [~A] appears" (join-strings cur-gram)))
      ;; [0, diff-pos], [0, diff-pos+1], ..., [0, n] of `pre-grams' will not appear again
      (unless (or (>= diff-pos m) (%malformed-gram pre-gram m bos eos))
        ;; only collect new grams, and skip grams full of `bos'
        (if (<= m running-pos)
            (push (format nil "~A ~A" (join-strings (subseq pre-gram 0 m)) running-total) gramlst)
            (push (format nil "~A ~A" (join-strings (subseq pre-gram 0 m)) pre-freq) gramlst)))
      (if (= diff-pos 0)
          (setf running-total cur-freq)
          (incf running-total cur-freq))
      (setf running-pos diff-pos)
      (setf pre-gram cur-gram
            pre-freq cur-freq))
    ;; one last gram
    (unless (or (>= diff-pos m) (%malformed-gram pre-gram m bos eos))
      (if (<= m running-pos)
          (push (format nil "~A ~A" (join-strings (subseq pre-gram 0 m)) running-total) gramlst)
          (push (format nil "~A ~A" (join-strings (subseq pre-gram 0 m)) pre-freq) gramlst)))
    (nreverse gramlst)))

(defun %collect-ngram-eq (ngram)
  "collect `ngram' into a list"
  (let ((gramlst nil))
    (labels ((collect (gramkey freq)
               (push (format nil "~A ~A" gramkey freq) gramlst)))
      (maphash #'collect ngram))
    gramlst))

(defun %sorted-ngram (ngram n m &key (bos "<BOS>") (eos "<EOS>") (idngram-p nil))
  "reduce `ngram' of size n to size m, and sort according to alphabetical order"
  (cond
    ((< m n)
     (%sorted-reduce-ngram ngram n m bos eos :idngram-p idngram-p))
    ((= m n)
     (if idngram-p
         (sort (%collect-ngram-eq ngram) #'%idnstr<)
         (sort (%collect-ngram-eq ngram) #'string<)))
    ((> m n)
     (error "Requires: m <= n"))))

(defun sorted-wngram (wngram n m &key (bos "<BOS>") (eos "<EOS>"))
  (%sorted-ngram wngram n m :bos bos :eos eos :idngram-p nil))

(defun sorted-idngram (idngram n m &key (bos "-1") (eos "-2"))
  (%sorted-ngram idngram n m :bos bos :eos eos :idngram-p t))
