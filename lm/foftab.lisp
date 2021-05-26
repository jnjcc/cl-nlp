;;;; Copyright (c) 2012-2015 jnjcc, Yste.org. All rights reserved.
;;;;
;;;; frequency of frequency table for ngram

(in-package #:cl-nlp)

;;; m-gram list reference with index starting from 0:
;;;   - proxy list reference for each m-gram (0 <= m <= n) of n-gram
;;;     + list elements: some statistics about a certain m-gram
;;;   - proxy list reference for n-gram
;;;     + list elements: each element of n-gram
(defun mlist-init (n &key (initial-element 0))
  "NOTICE: reserve place for a fake 0-gram for convenience"
  (make-list (+ n 1) :initial-element initial-element))

(defun mlist-copy (mlist)
  (copy-list mlist))

(defun mlist-ref (mlist m)
  (nth m mlist))

(defun mlist-setf (mlist m val)
  (setf (nth m mlist) val))

(defun mlist-incf (mlist m &optional (delta 1))
  (incf (nth m mlist) delta))

;;; m-gram list-of-list, with outer index starting from 1, inner index starting from 0:
;;;   - proxy list-of-list reference for each m-gram (1 <= m <= n) of n-gram
;;;     + outer: a certain m-gram; inner: each element of m-gram, or a `mlist' as above
(defun lol-init (n)
  "NOTICE: for each m-gram, reserve an extra 0-th m-gram for convenience"
  (let ((lol (make-list n :initial-element nil)))
    (dotimes (i n)
      (setf (nth i lol) (make-list 1 :initial-element 0)))
    lol))

(defun lol-get (lol m)
  "get info about a certain m-gram, return a `mlist'"
  (nth (- m 1) lol))

;; combined `lol-get' and `lol-mref'
(defun lol-ref (lol m idx)
  "access info (e.g., frequency) for `idx'-th m-gram,
where `idx' starts from 0, which is just for convenience"
  (nth idx (nth (- m 1) lol)))

(defun lol-aset (lol m cur-len val)
  "we are appending (and setting) each m-gram one by one"
  ;; remember the extra 0-th m-gram, thus total length will be (cur-len + 1)
  (if (= (length (nth (- m 1) lol)) (+ cur-len 1))
      (setf (nth (- m 1) lol) (append (nth (- m 1) lol) (list val)))
      (error "something bad happens...")))

(defun lol-setf (lol m idx val)
  (setf (nth idx (nth (- m 1) lol)) val))

;;; frequency of frequency table for n-gram language model
;;;   truncated `list-of-list' (by *fof-disc-range* and *fof-uni-range*)
(defvar *fof-disc-range* 7)

(defvar *fof-uni-range* 3)

(defun fof-init (n &optional (disc-range *fof-disc-range*) (uni-range *fof-uni-range*))
  (let ((fof (make-list n :initial-element 0)))
    (dotimes (i (length fof))
      (if (= i 0)
          (setf (nth 0 fof) (make-list (+ uni-range 2) :initial-element 0))
          ;; we also need to remember the frequency of frequency 0 and (+ disc-range 1)
          ;;   for discounting purpose, thus the "+2" operation
          (setf (nth i fof) (make-list (+ disc-range 2) :initial-element 0))))
    fof))

(defun fof-disc (fof m)
  "return discount range for m-gram"
  (- (length (nth (- m 1) fof)) 2))

(defun fof-copy-init (fof)
  "copy frequency of frequency table and initialize discount ratio to 1.0"
  (let ((copy (make-list (length fof) :initial-element 1.0)))
    (dotimes (i (length fof))
      (setf (nth i copy) (make-list (length (nth i fof)) :initial-element 1.0)))
    copy))

(defun fof-ref (fof m freq)
  "frequency of frequency `freq' in `m' gram"
  (when (< freq (length (nth (- m 1) fof)))
    (nth freq (nth (- m 1) fof))))

(defun fof-setf (fof m freq val)
  (when (< freq (length (nth (- m 1) fof)))
    (setf (nth freq (nth (- m 1) fof)) val)))

(defun fof-incf (fof m freq &optional (delta 1))
  (when (< freq (length (nth (- m 1) fof)))
    (incf (nth freq (nth (- m 1) fof)) delta)))

(defun fof-from-ngram (ngram n &key (disc *fof-disc-range*) (uni *fof-uni-range*) (bos "-1") (eos "-2"))
  "Only discount frequencies less than discount-range `disc' for m-grams (m > 1),
the belief being that events occurring more than `disc' times are well estimated by MLE
Requires: `bos' and `eos' non-NIL"
  (let* ((fof (fof-init n disc uni))
         (sorted (sorted-extend ngram n :eos eos)) ;; it is ok we sort by alphabetical order even if idngram
         (pre-gram (caar sorted))
         (pre-freq (cdar sorted))
         (m 1)
         ;; `running-total' for 1, 2, ..., n-gram (NOTICE: each m-gram's are sorted)
         (running-total (mlist-init n :initial-element pre-freq))
         (cur-gram nil)
         (cur-freq nil)
         (diff-pos 0))
    (dotimes (i (- (length sorted) 1))
      ;; starting from 1-th index
      (setf cur-gram (car (nth (+ i 1) sorted))
            cur-freq (cdr (nth (+ i 1) sorted)))
      (setf diff-pos n)
      (dotimes (j n)
        (when (string/= (nth j cur-gram) (nth j pre-gram))
          (setf diff-pos j)
          (return)))
      (when (= diff-pos n)
        (error "same gram [~A] appears" (join-strings cur-gram)))
      (dotimes (d n)
        ;; `pre-gram'[0, d], which is (d+1)-gram
        (setf m (+ d 1))
        (unless (%malformed-gram pre-gram m bos eos)
          ;; [0, m), [0, m+1), ..., [0, n) of `cur-gram' are new m-grams
          (when (> m diff-pos)
            (fof-incf fof m (mlist-ref running-total m))
            (mlist-setf running-total m 0))
          (mlist-incf running-total m cur-freq)))
      (setf pre-gram cur-gram))
    ;; one last gram
    (dotimes (d n)
      (setf m (+ d 1))
      (unless (%malformed-gram pre-gram m bos eos)
        (fof-incf fof m (mlist-ref running-total m)))
      (mlist-setf running-total m 0))
    fof))
