;;;; Copyright (c) 2012-2015 jnjcc, Yste.org. All rights reserved.
;;;;
;;;; Katz Backoff Model

(in-package #:cl-nlp)

(defun %get-next-index (idx m idminfo)
  "given `idx' as the current m-gram, what is the latest (m+1)-gram it points to?"
  (if (= idx (idm-get-gnum idminfo m))
      ;; points to the last (m+1)-gram
      (idm-get-gnum idminfo (+ m 1))
      (idm-get-ptrs idminfo m idx)))

(defun %binary-search (idminfo m beg end wdid)
  (do ((mid nil))
      ((> beg end))
    (setf mid (+ beg (floor (- end beg) 2)))
    (cond
      ((< wdid (idm-get-wdid idminfo m mid)) (setf end (- mid 1)))
      ((> wdid (idm-get-wdid idminfo m mid)) (setf beg (+ mid 1)))
      ((= wdid (idm-get-wdid idminfo m mid)) (return-from %binary-search mid))))
  (return-from %binary-search nil))

(defun %find-sub-gram (idlist h idminfo curidxs)
  "Given `idlist' (H, w), where H is of length h, find sub-gram and write index into `curidxs'
e.g. let `idlist' = (6 5 2), we want to know if (6), (6 5) and (6 5 2) exists in turn,
which is of length 1, 2, ..., (h+1)"
  (let ((glen 0)
        (exists nil))
    (do ((m 1 (+ m 1)))
        ((> m (+ h 1)))
      (setf exists nil)
      (cond
        ((= m 1)
         (when (> (idm-get-freq idminfo 1 (car idlist)) 0)
           (setf exists t)
           (mlist-setf curidxs 1 (car idlist))))
        ((> m 1)
         (let* ((pm (- m 1))
                (idx (mlist-ref curidxs pm))
                ;; `idx'-th (m-1)-gram spans from (`beg'+1) to `end'-th m-gram
                (beg (%get-next-index (- idx 1) pm idminfo))
                (end (%get-next-index idx pm idminfo)))
           (incf beg)
           (setf exists (%binary-search idminfo m beg end (nth (- m 1) idlist)))
           (when exists
             (mlist-setf curidxs m exists)))))
      (if exists
          ;; Hooray! current m-gram exists!
          (incf glen)
          (return)))
    glen))

(defun katz-backoff (idlist h idminfo)
  "pKatz(w|H) for `idlist' (H, w), where H is of length h"
  (let ((prob nil)
        (mgfreq 0.0) ;; C(H, w)
        (hgfreq 0.0) ;; C(H)
        (alpha 0.0))
    (if (= 0 h)
        ;; no history, fall back to unigram probability
        (setf prob (idm-get-uprob idminfo (car idlist)))
        (let ((curidxs (mlist-init (+ h 1) :initial-element 0))
              (glen 0)) ;; sub gram length found
          (setf glen (%find-sub-gram idlist h idminfo curidxs))
          (cond
            ((= glen (+ h 1)) ;; C(H, w) > 0, and thus C(H) > 0
             (progn
               (setf mgfreq (idm-get-freq idminfo glen (mlist-ref curidxs glen)))
               (setf mgfreq (idm-discount-freq idminfo glen mgfreq))
               (setf hgfreq (idm-get-freq idminfo h (mlist-ref curidxs h)))
               (setf prob (/ mgfreq hgfreq))))
            ((>= glen h) ;; C(H) > 0
             (progn
               (setf hgfreq (idm-get-freq idminfo h (mlist-ref curidxs h)))
               (setf alpha (idm-get-alpha idminfo h (mlist-ref curidxs h)))
               (setf prob (katz-backoff (cdr idlist) (- h 1) idminfo))
               (setf prob (* alpha prob))))
            ((< glen h) ;; C(H) < 0
             (progn
               (setf prob (katz-backoff (cdr idlist) (- h 1) idminfo)))))
          (when (= 100 h)
            (format t "length: ~A; P(~A | " glen (car (last idlist)))
            (dolist (elem (butlast idlist))
              (format t "~A " elem))
            (format t ") = ~,5f~%" prob))
          ))
    (when (= 100 h)
      (format t "length: ~A; P(~A | ) = ~,5f~%" 1 (car (last idlist)) prob))
    prob))
