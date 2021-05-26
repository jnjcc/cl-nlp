;;;; Copyright (c) 2012-2015 jnjcc, Yste.org. All rights reserved.
;;;;
;;;; Dictionary (for string) using association list / hash-table
;;;;   (cl-ml::bins) mainly used for non-string keys

(in-package #:cl-nlp/algo)

;;; Dictionary using association list
(defun make-adict ()
  nil)

(defun adict-len (adict)
  (length adict))

;; similar to `binincf'
(defmacro adict-add (adict key value)
  `(if (assoc ,key ,adict :test #'string=)
       (setf (cdr (assoc ,key ,adict :test #'string=)) ,value)
       (push (cons ,key ,value) ,adict)))

;; similar to `binpush', append `value' to value list of `key'
(defmacro adict-append (adict key value)
  `(if (assoc ,key ,adict :test #'string=)
       (setf (cdr (assoc ,key ,adict :test #'string=))
             (append (cdr (assoc ,key ,adict :test #'string=))
                     (list ,value)))
       (push (cons ,key (list ,value)) ,adict)))

(defun zip-adict (keys values)
  (mapcar #'cons keys values))

(defun adict-ref (adict key)
  (cdr (assoc key adict :test #'string=)))

;;; Dictionary using hash-table
(defun make-hdict ()
  (make-hash-table :test 'equal))

(defun hdict-len (hdict)
  (hash-table-count hdict))

(defun hdict-add (hdict key value)
  (unless value
    (error "NOTICE: we do not allow nil values in hdict"))
  (setf (gethash key hdict) value))

(defun hdict-append (hdict key value)
  (multiple-value-bind (val exist) (gethash key hdict)
    (unless exist
      (setf val nil))
    (setf val (append val (list value)))
    (setf (gethash key hdict) val)))

(defun zip-hdict (keys values)
  (let ((hdict (make-hdict)))
    (dotimes (i (length keys))
      (hdict-add hdict (nth i keys) (nth i values)))
    hdict))

(defun hdict-ref (hdict key)
  ;; nil value not allowed during `hdict-add'
  (gethash key hdict))
