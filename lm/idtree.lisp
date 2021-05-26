;;;; Copyright (c) 2012-2015 jnjcc, Yste.org. All rights reserved.
;;;;
;;;; m-gram statistics and trees for n-gram (m <= n)

(in-package #:cl-nlp)

(defun %collect-grams (pre-gram n cur-gram cur-freq diff bos eos running-total idm)
  "`cur-gram' and `pre-gram' differs starting from position `diff',
which means [0, `diff'], [0, `diff'+1], ..., [0, n] of `cur-gram' are brand new grams,
we need to update `running-total', and collect `pre-gram' into `idm'"
  (let ((pre-idx (parse-integer (car pre-gram) :junk-allowed t))
        (cur-idx (parse-integer (car cur-gram) :junk-allowed t))
        (m 1))
    (dotimes (d n)
      ;; looping for n, (n-1), ..., 1 grams
      (setf m (- n d))
      (unless (%malformed-gram pre-gram m bos eos)
        ;; e.g., for trigrams: (NOTICE: what if "5 6 7 30" is the last line?)
        ;;   1 2 3 10
        ;;   1 2 4 20
        ;;   5 6 7 30
        (when (> m diff)
          ;; TODO: Good-Turing, etc
          (idm-incf-fof idm m (mlist-ref running-total m))
          ;; say, we are processing "1 2 4", where `diff'=2
          ;;   now that "1 2 3" being the 1-st trigram, we want to remember its frequency,
          ;;   its last wordid, and its backoff alpha
          (idm-aset-gram idm m (mlist-ref running-total m)
                         (parse-integer (nth (- m 1) pre-gram) :junk-allowed t) 1.0)
          (cond
            ((= m n)
             (idm-incf-gnum idm m))
            ((> m 1)
             ;; we want to remember the 1-st bigram "1 2" points to the 2-nd trigram "1 2 4"
             ;;   which is the latest trigram it points to
             (progn
               (idm-aset-ptrs idm m)
               (idm-incf-gnum idm m)))
            ((= m 1)
             ;; when we are process the 3-rd trigram "3 4 5", we also want to remember that
             ;;   the uni-grams "1", "2", "3", "4" points to the 2-nd bigram "1 2"
             ;;   NOTICE: this only happens if we didn't do `sorted-extend' of <EOS>
             (do ((idx pre-idx (+ idx 1)))
                 ((>= idx cur-idx))
               (idm-aset-ptrs idm 1)
               (idm-incf-gnum idm 1))))
          (mlist-setf running-total m 0))
        (mlist-incf running-total m cur-freq)))))

(defun idngram-tree (idngram n idmax &key (disc *fof-disc-range*) (uni *fof-uni-range*) (bos "-1") (eos "-2"))
  (let* ((idm (make-instance 'idinfo-type :disc-range disc :uni-range uni :n n))
         (sorted (sorted-extend idngram n :eos eos :idngram-p t))
         (pre-gram (caar sorted))
         (pre-freq (cdar sorted))
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
      (%collect-grams pre-gram n cur-gram cur-freq diff-pos bos eos running-total idm)
      (setf pre-gram cur-gram))
    ;; one last gram
    (%collect-grams pre-gram n cur-gram cur-freq 0 bos eos running-total idm)
    ;; NOTICE: for the last gram "5 6 7 30", we have to complete the tree
    (do ((id (parse-integer (car cur-gram) :junk-allowed t) (+ id 1)))
        ((> id idmax))
      (idm-aset-ptrs idm 1)
      (idm-incf-gnum idm 1))
    idm))
