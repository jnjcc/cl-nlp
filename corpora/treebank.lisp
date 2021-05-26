;;;; Copyright (c) 2012-2015 jnjcc, Yste.org. All rights reserved.
;;;;
;;;; NOTICE: Wall Street Journal treebank from NLTK
;;;;   - wsj_0001.mrg: . and `..' and `...', etc
;;;;   - wsj_0008.mrg: 's of "it's"
;;;;   - wsj_0088.mrg: colon in `3:15'
;;;;   - wsj_0118.mrg: `ADVP|PRT'
;;;; NOTICE: some other cases:
;;;;   - wsj_0192.mrg: `377.60' reduced to `377.6'

(in-package #:cl-nlp)

;;; Reader macros stuff
(defvar *punctuations* '(#\, #\. #\; #\# #\' #\` #\: #\|))

(defun stringify (object)
  (etypecase object
    (list object)
    (t (format nil "~A" object))))

(defun read-next-object (stream)
  (let ((next (peek-non-space stream t nil)))
    (cond
      ((char= #\) next) (discard-next-char stream))
      ((or (char= #\: next) (char= #\. next))
       ;; NOTICE: for `:abc' and `...'
       (let ((chars (read-chars-until stream (lambda (x) (or (space-char-p x) (char= #\) x))))))
         (join-strings chars "")))
      (t (read stream t nil t)))))

(defun read-left-bracket (stream char)
  (declare (ignore char))
  (loop for object = (read-next-object stream)
     while object
     collect (stringify object) into objects
     finally (return objects)))

(defmacro with-treebank-readtable ((readtable) &body body)
  `(let ((*readtable* (copy-readtable))
         (,readtable nil))
     (multiple-value-bind (afunc non-terminating-p) (get-macro-character #\a)
       (declare (ignore non-terminating-p))
       (dolist (punct *punctuations*)
         ;; with `non-terminating-p' being t, we are allowed to read:
         ;;   - `ADVP|PRT' or `A"B' or '''
         ;; NOTICE: but _NOT_ for `3:15' and `..'
         (set-macro-character punct afunc t))
       (set-macro-character #\( #'read-left-bracket)
       (setf (readtable-case *readtable*) :invert))
     (setf ,readtable *readtable*)
     ,@body))

;;; WSJ treebank 1: load treebank using reader macros
(defun read-treebank-sexp (stream)
  "read one treebank sexp from `treebank/combined/wsj_0*.mrg'"
  (with-treebank-readtable (rtable)
    (read stream)))

(defun load-treebank (stream)
  "load treebank sexps into a list"
  (with-treebank-readtable (rtable)
    (let ((treebank nil))
      (do ((sexp (read stream nil :eof) (read stream nil :eof)))
          ((eq sexp :eof))
        (push sexp treebank))
      (nreverse treebank))))

(defun load-treebank-file (fpath)
  (with-open-file (fp fpath)
    (load-treebank fp)))

;;; WSJ treebank 2: treebank using search-and-replace
(defun read-treebank-regex (fpath)
  (let* ((content (read-file-content fpath))
         (seqs (split-string content #\Space)))
    (labels ((rewrite (word)
               ;; NOTICE: for `3:15', etc
               (replace-string-if word (lambda (ch) (char= ch #\:)) "\\:")))
      (join-strings (mapcar #'rewrite seqs) #\Space))))

(defun load-treebank-regex (fpath)
  (let ((regex (read-treebank-regex fpath)))
    (with-input-from-string (stream regex)
      (load-treebank stream))))

;; TODO: this is ugly in order to process `wsj_0088.mrg' and `wsj_0118.mrg'
(defun load-treebank-dir (dir)
  "`dir' = `treebank/combined/'"
  (let ((treebank nil))
    (setf dir (format nil "~A/wsj_*.mrg" dir))
    (dolist (path (directory dir))
      (if (or (search "wsj_0088.mrg" (file-namestring path))
              (search "wsj_0118.mrg" (file-namestring path)))
          (setf treebank (append treebank (load-treebank-regex path)))
          (setf treebank (append treebank (load-treebank-file path)))))
    treebank))

(defun %parse-treebank-sexp (sexp tagged)
  (dolist (subtree sexp)
    (when (listp subtree)
      (if (and (= (length subtree) 2)
               (atom (first subtree))
               (atom (second subtree)))
          (push (cons (second subtree) (first subtree)) tagged)
          (setf tagged (%parse-treebank-sexp subtree tagged)))))
  tagged)

(defun parse-treebank-sexp (sexp)
  (let ((tagged nil))
    (nreverse (%parse-treebank-sexp sexp tagged))))

(defun parse-treebank (treebank)
  (let ((tagged nil))
    (dolist (sexp treebank)
      (push (parse-treebank-sexp sexp) tagged))
    (nreverse tagged)))
