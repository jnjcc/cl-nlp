;;;; Copyright (c) 2012-2015 jnjcc, Yste.org. All rights reserved.
;;;;
;;;; Edit Distance

(in-package #:cl-nlp/algo)

(defun edit-distance (lst1 lst2 &key (equal #'string=))
  (let ((m (length lst1))
        (n (length lst2))
        (leven nil))
    (setf leven (make-array (list (+ m 1) (+ n 1)) :initial-element 0))
    ;; leven[i][0]
    (dotimes (i (+ m 1))
      (setf (aref leven i 0) i))
    ;; leven[0][j]
    (dotimes (j (+ n 1))
      (setf (aref leven 0 j) j))
    (let ((rplce nil)
          (i nil) (j nil))
      (dotimes (l1 m)
        ;; leven[1:][1:]
        (setf i (+ l1 1))
        (dotimes (l2 n)
          (setf j (+ l2 1))
          (if (funcall equal (nth l1 lst1) (nth l2 lst2))
              (setf rplce 0)
              (setf rplce 1))
          (setf (aref leven i j)
                (min (+ (aref leven (- i 1) j) 1)
                     (+ (aref leven i (- j 1)) 1)
                     (+ (aref leven (- i 1) (- j 1)) rplce))))))
    (aref leven m n)))
