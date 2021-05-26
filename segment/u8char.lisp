;;;; Copyright (c) 2012-2015 jnjcc, Yste.org. All rights reserved.
;;;;
;;;; UTF-8 characters

(in-package #:cl-nlp)

(defun %segment-keep-alnum (u8str)
  (let ((chars nil)
        (len (length u8str))
        ;; Finite state machine
        (state :empty)
        (beg 0)
        (chr nil))
    (dotimes (i len)
      (setf chr (char u8str i))
      (cond
        ((eq state :empty)
         (cond
           ((alpha-char-p chr) (setf state :alpha))
           ((digit-char-p chr) (setf state :digit))
           (t (setf state :u8char))))
        ((eq state :alpha)
         (cond
           ((alpha-char-p chr) (setf state :alpha))
           (t (progn
                (push (subseq u8str beg i) chars)
                (setf beg i)
                (if (digit-char-p chr)
                    (setf state :digit)
                    (setf state :u8char))))))
        ((eq state :digit)
         (cond
           ((digit-char-p chr) (setf state :digit))
           (t (progn
                (push (subseq u8str beg i) chars)
                (setf beg i)
                (if (alpha-char-p chr)
                    (setf state :alpha)
                    (setf state :u8char))))))
        ((eq state :u8char)
         (progn
           (push (subseq u8str beg i) chars)
           (setf beg i)
           (cond
             ((alpha-char-p chr) (setf state :alpha))
             ((digit-char-p chr) (setf state :digit))
             (t (setf state :u8char)))))))
    (when (> len 0)
      (push (subseq u8str beg len) chars))
    (nreverse chars)))

(defun %segment-all-chars (u8str)
  (let ((chars nil)
        (len (length u8str)))
    (dotimes (i len)
      (push (subseq u8str i (+ i 1)) chars))
    (nreverse chars)))

(defun segment-chars (u8str &key (keep-alnum t))
  (if keep-alnum
      (%segment-keep-alnum u8str)
      (%segment-all-chars u8str)))
