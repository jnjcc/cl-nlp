;;;; Copyright (c) 2012-2015 jnjcc, Yste.org. All rights reserved.
;;;;
;;;; vocabulary as hash-table: word -> id

(in-package #:cl-nlp)

(defun update-word-id (vocab word newid)
  "exist or not, we update word id"
  (multiple-value-bind (oldid exist) (gethash word vocab)
    (when exist
      (format t "~A already exist with id ~A, overwritting" word oldid))
    (setf (gethash word vocab) newid)))

(defun wfreq-idvocab (wfreq &key (sortby :key))
  "hash-table: word -> id; id=0 being OOV words"
  (let ((wlst (sorted-words wfreq :compare #'> :filter :key :minfreq 0))
        (vocab (make-hash-table :test 'equal))
        (id 1))
    (when (eq sortby :key)
      (setf wlst (sort wlst #'string<)))
    (dolist (word wlst)
      (update-word-id vocab word id)
      (incf id))
    (values vocab (- id 1))))

(defun read-idvocab (stream)
  (let ((vocab (make-hash-table :test 'equal))
        (id 1))
    (do ((word (read-line stream nil :eof) (read-line stream nil :eof)))
        ((eq word :eof))
      (update-word-id vocab word id)
      (incf id))
    (values vocab (- id 1))))
