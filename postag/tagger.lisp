;;;; Copyright (c) 2012-2015 jnjcc, Yste.org. All rights reserved.
;;;;
;;;; Part-of-Speech tagging

(in-package #:cl-nlp)

;;; observations:
;;;   - sentence: (word word ...)
;;;   - corpus: (sentence sentence ...)
;;;   - tagged sentence: ((word . tag) ...)
;;;   - tagged corpus: (tagsent tagsent ...)
(defclass pos-tagger ()
  ())

(defun untag-sentence (tagsent)
  "tagged sentence -> sentence"
  (mapcar #'car tagsent))

(defun tagsent-tags (tagsent)
  "tagged sentence -> tags"
  (mapcar #'cdr tagsent))

(defun untag-corpus (tagcorpus)
  (mapcar #'untag-sentence tagcorpus))

(defun flatten-tags (tagcorpus)
  (reduce #'append (mapcar #'tagsent-tags tagcorpus)))

(defgeneric pos-tag-sent (pos sent)
  (:documentation "tag sentence `sent'"))

(defmethod pos-tag-corpus ((pos pos-tagger) sents)
  (let ((tagged nil))
    (dolist (sent sents)
      (push (pos-tag-sent pos sent) tagged))
    (nreverse tagged)))

(defun accuracy-tags (taglabl tagpred)
  (let ((acc 0) (num (length taglabl)))
    (dotimes (i num)
      (when (string= (nth i tagpred) (nth i taglabl))
        (incf acc)))
    (float (/ acc num))))

(defmethod pos-evaluate ((pos pos-tagger) tagcorpus)
  (let* ((corpus (untag-corpus tagcorpus))
         (pred (pos-tag-corpus pos corpus)))
    (let ((taglabl (flatten-tags tagcorpus))
          (tagpred (flatten-tags pred)))
      (accuracy-tags tagpred taglabl))))
