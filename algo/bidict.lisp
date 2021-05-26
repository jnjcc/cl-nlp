;;;; Copyright (c) 2012-2015 jnjcc, Yste.org. All rights reserved.
;;;;
;;;; Bidirectional dictionary using association list / hash-table
;;;;   - forward: string -> int (which is unique, mainly used as indices)
;;;;   - backward: int -> string

(in-package #:cl-nlp/algo)

;;; Bidirectional dictionary using association list
(defun make-badict ()
  (list (make-adict) (make-adict)))

(defun badict-len (badict)
  (adict-len (car badict)))

(defmacro badict-add (badict key value)
  (let ((old-sym (gensym "OVAL-")))
    `(let ((,old-sym (adict-ref (car ,badict) ,key)))
       (adict-add (car ,badict) ,key ,value)
       (when ,old-sym
         (setf (cadr ,badict) (remove-if (lambda (x) (eq (car x) ,old-sym)) (cadr ,badict))))
       (push (cons ,value ,key) (cadr ,badict)))))

(defun zip-badict (keys values)
  (list (zip-adict keys values)
        (zip-adict values keys)))

(defun badict-ref (badict key)
  (adict-ref (car badict) key))

(defun badict-bref (badict value)
  "backward reference"
  (cdr (assoc value (cadr badict))))

;;; Bidirectional dictionary using hash-table
(defun make-bhdict ()
  (list (make-hdict) (make-hdict)))

(defun bhdict-len (bhdict)
  (hdict-len (car bhdict)))

(defun bhdict-add (bhdict key value)
  (unless value
    (error "NOTICE: we do not allow nil values in bhdict"))
  (setf (gethash key (car bhdict)) value
        (gethash value (cadr bhdict)) key))

(defun zip-bhdict (keys values)
  (let ((bhdict (make-bhdict)))
    (dotimes (i (length keys))
      (bhdict-add bhdict (nth i keys) (nth i values)))
    bhdict))

(defun bhdict-ref (bhdict key)
  (hdict-ref (car bhdict) key))

(defun bhdict-bref (bhdict value)
  (hdict-ref (cadr bhdict) value))
