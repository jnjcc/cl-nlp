;;;; Copyright (c) 2012-2015 jnjcc, Yste.org. All rights reserved.
;;;;
;;;; CoNLL-2000 Shared Task Chunking, for POS tagging

(in-package #:cl-nlp)

;;; CoNLL-2000 Chunking (file format: word pos chunk)
;;;   - `num': `w' full of digit; `cap': first letter uppercase; `sym': no alnum chars
;;;   - `p3': prefix of `w', i.e. `w[0:3]'; `s3': suffix
(defvar *conll-pos-fields*
  '("w" "pos" "chunk" "num" "cap" "sym" "p1" "p2" "p3" "p4" "s1" "s2" "s3" "s4"))

(defvar *conll-pos-label* "pos")

(defvar *conll-pos-templates*
  '(
    (("num" . 0))
    (("cap" . 0))
    (("sym" . 0))
    (("p1"  . 0))
    (("p2"  . 0))
    (("p3"  . 0))
    (("p4"  . 0))
    (("s1"  . 0))
    (("s2"  . 0))
    (("s3"  . 0))
    (("s4"  . 0))

    (("w" .  0))
    (("w" . -1))
    (("w" .  1))
    (("w" . -2))
    (("w" .  2))
    (("w" . -2) ("w" . -1))
    (("w" . -1) ("w" .  0))
    (("w" .  0) ("w" .  1))
    (("w" .  1) ("w" .  2))
    (("w" . -2) ("w" . -1) ("w" .  0))
    (("w" . -1) ("w" .  0) ("w" .  1))
    (("w" .  0) ("w" .  1) ("w" .  2))
    (("w" . -2) ("w" . -1) ("w" .  0) ("w" .  1))
    (("w" . -1) ("w" .  0) ("w" .  1) ("w" .  2))
    (("w" . -2) ("w" . -1) ("w" .  0) ("w" .  1) ("w" .  2))

    (("w" .  0) ("w" . -1))
    (("w" .  0) ("w" . -2))
    (("w" .  0) ("w" . -3))
    (("w" .  0) ("w" . -4))
    (("w" .  0) ("w" . -5))
    (("w" .  0) ("w" . -6))
    (("w" .  0) ("w" . -7))
    (("w" .  0) ("w" . -8))
    (("w" .  0) ("w" . -9))

    (("w" .  0) ("w" .  1))
    (("w" .  0) ("w" .  2))
    (("w" .  0) ("w" .  3))
    (("w" .  0) ("w" .  4))
    (("w" .  0) ("w" .  5))
    (("w" .  0) ("w" .  6))
    (("w" .  0) ("w" .  7))
    (("w" .  0) ("w" .  8))
    (("w" .  0) ("w" .  9))
    ))

;; NOTICE: tricky, not recommended: it is better to first preprocess the file
(defun expand-cword-pos (cword)
  "`cword' being (word pos chunk), we need to expand in order the following feats:
[num, cap, sym, p1, p2, p3, p4, s1, s2, s3, s4]"
  (let* ((word (car cword))
         (wlen (length word))
         (wfeat nil))
    (labels ((sym-word-p (word)
               (every (lambda (chr) (not (alphanumericp chr))) word)))
      (dolist (fn (list #'digit-word-p #'title-word-p #'sym-word-p))
        (if (funcall fn word)
            (push "1" wfeat)
            (push "0" wfeat))))
    (loop for pf from 1 to 4 do
         (if (>= wlen pf)
             (push (subseq word 0 pf) wfeat)
             (push "" wfeat)))
    (loop for sf from 1 to 4 do
         (if (>= wlen sf)
             (push (subseq word (- wlen sf) wlen) wfeat)
             (push "" wfeat)))
    (append cword (nreverse wfeat))))

(defun write-conll-pos (istream &optional (ostream t))
  (let ((*conll-line-expand* #'expand-cword-pos))
    (write-conll-feats istream *conll-pos-fields* *conll-pos-label*
                       *conll-pos-templates* ostream)))
