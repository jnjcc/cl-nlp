;;;; Copyright (c) 2012-2015 jnjcc, Yste.org. All rights reserved.
;;;;
;;;; CoNLL-2000 Shared Task Chunking
;;;;   (https://www.clips.uantwerpen.be/conll2000/chunking/)

(in-package #:cl-nlp)

;;; CoNLL-2000 Chunking
;;;   file format: word pos chunk
(defvar *conll-2000-fields* '("w" "pos" "chunking"))

(defvar *conll-chunk-label* "chunking")

(defvar *conll-word-feats* "__FEATS__")

;; extract features from each CoNLL sentence
(defvar *conll-chunk-templates*
  '(
    (("w" . -2)) ;; what is the [-2] word? "w" being CoNLL field, -2 being offset
    (("w" . -1))
    (("w" . +0)) ;; what is current word?
    (("w" . +1))
    (("w" . +2))
    (("w" . -1) ("w" . 0)) ;; what is prev word and current word?
    (("w" . +0) ("w" . 1))
    (("pos" . -2))
    (("pos" . -1))
    (("pos" . +0))
    (("pos" . +1))
    (("pos" . +2))
    (("pos" . -2) ("pos" . -1))
    (("pos" . -1) ("pos" . +0))
    (("pos" . +0) ("pos" . +1))
    (("pos" . +1) ("pos" . +2))
    (("pos" . -2) ("pos" . -1) ("pos" . 0))
    (("pos" . -1) ("pos" . +0) ("pos" . 1))
    (("pos" . +0) ("pos" . +1) ("pos" . 2))
    ))

;; NOTICE: tricky, not recommended
(defvar *conll-line-expand* nil)

;;; CoNLL "word" as adict:
;;;   - e.g. (("w" . "Confidence") ("pos" . "NN") ("chunking" . "B-NP") ("__FEATS__" ()))
;;;   - where ("w" "pos" "chunking") comes from `fields', "__FEATS__" as features
;;; CoNLL "sentence":
;;;   - a list of CoNLL "word"
(defun read-conll-sent (stream fields)
  "read one full sentence from CoNLL dataset, which is a list of CoNLL 'word'
Returns: :eof if no more CoNLL sentence; or list of CoNLL words"
  (let ((sent nil))
    (when (stream-eof-p stream)
      (return-from read-conll-sent :eof))
    ;; collect one line of CoNLL corpus into a list `cword'
    (do-sentence (cword stream :eow #\Space :eol #\Newline)
      (when (null cword)
        (return))
      (when *conll-line-expand*
        (setf cword (funcall *conll-line-expand* cword)))
      (push (zip-adict fields cword) sent))
    (nreverse sent)))

(defun %sent-ref (sent idx field)
  "`field' of `idx'-th CoNLL word of `sent'"
  (adict-ref (nth idx sent) field))

(defun %sent-append (sent idx value)
  "append `value' to `idx'-th CoNLL word's feature"
  (when (< idx 0)
    (setf idx (+ idx (length sent))))
  (adict-append (nth idx sent) *conll-word-feats* value))

(defun escape-colon (str)
  (replace-string str #\: "__COLON__"))

(defun parse-conll-sent (sent templates)
  "parse CoNLL sentence, extract features, and write into `*conll-word-feats*' slot"
  (dolist (template templates)
    (let ((name (join-strings
                 ;; w[-1]|w[0], etc
                 (mapcar (lambda (x) (format nil "~A[~A]" (car x) (cdr x))) template)
                 #\|))
          (field nil)
          (idx nil)
          ;; `name'=`values'
          (values nil))
      (dotimes (ts (length sent))
        (setf values nil)
        (dolist (pair template)
          (setf field (car pair))
          (setf idx (+ ts (cdr pair)))
          (when (or (< idx 0) (>= idx (length sent)))
            (setf values nil)
            (return))
          (push (%sent-ref sent idx field) values))
        (when values
          (%sent-append sent ts
                        (format nil "~A=~A" (escape-colon name)
                                (escape-colon (join-strings (nreverse values) "|"))))))))
  (%sent-append sent 0 "__BOS__")
  (%sent-append sent -1 "__EOS__")
  sent)

(defun read-parsed-conll (stream fields templates)
  (let ((sent (read-conll-sent stream fields)))
    (unless (eq sent :eof)
      (parse-conll-sent sent templates))
    sent))

(defun write-conll-feats (istream fields label templates &optional (ostream t))
  (do ((sent (read-parsed-conll istream fields templates)
             (read-parsed-conll istream fields templates)))
      ((eq sent :eof))
    (dolist (cword sent)
      (format ostream "~A" (adict-ref cword label))
      (dolist (feat (adict-ref cword *conll-word-feats*))
        (format ostream "~A~A" #\Tab feat))
      (format ostream "~%"))
    (format ostream "~%")))

(defun write-conll-chunk (istream &optional (ostream t))
  (write-conll-feats istream *conll-2000-fields* *conll-chunk-label*
                     *conll-chunk-templates* ostream))
