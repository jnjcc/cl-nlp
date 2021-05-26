;;;; Copyright (c) 2012-2015 jnjcc, Yste.org. All rights reserved.
;;;;
;;;; Vocabulary for word2vec

(in-package #:cl-nlp)

(defclass w2v-word-info ()
  ((freq :initform 0 :initarg :freq :reader winfo-freq
         :documentation "word frequency")
   (indx :initform -1 :initarg :indx :reader winfo-indx
         :documentation "word to one-hot")
   (path :initform nil :reader winfo-path
         :documentation "huffman tree path from root to parent of leaf")
   (code :initform nil :reader winfo-code
         :documentation "huffman code: if left child 0, else 1")))

(defmethod print-object ((info w2v-word-info) stream)
  (with-slots (freq indx path code) info
    (format stream "freq: ~A; indx: ~A; code: ~A~%" freq indx code)
    (format stream "path:~A" path)))

(defmethod incf-freq ((info w2v-word-info) &optional (delta 1))
  (with-slots (freq) info
    (incf freq delta)))

(defmethod fill-huffman ((info w2v-word-info) plst binary)
  (with-slots (path code) info
    (setf path plst)
    (setf code binary)))

(defclass w2v-vocab ()
  ((word-hash :initform (make-hash-table :test 'equal) :reader word-hash)
   ;; one-hot to word
   (indx-hash :initform (make-hash-table :test 'equal))
   (vocab-size :initform 0 :reader vocab-size)))

(defmethod has-word-in-vocab ((vocab w2v-vocab) word)
  (with-slots (word-hash) vocab
    (nth-value 1 (gethash word word-hash))))

(defmethod get-word-info ((vocab w2v-vocab) word)
  (with-slots (word-hash) vocab
    (gethash word word-hash)))

(defmethod get-word-freq ((vocab w2v-vocab) word)
  (with-slots (word-hash) vocab
    (slot-value (gethash word word-hash) 'freq)))

(defmethod get-word-indx ((vocab w2v-vocab) word)
  (with-slots (word-hash) vocab
    (slot-value (gethash word word-hash) 'indx)))

(defmethod get-word-path ((vocab w2v-vocab) word)
  (with-slots (word-hash) vocab
    (slot-value (gethash word word-hash) 'path)))

(defmethod get-word-code ((vocab w2v-vocab) word)
  (with-slots (word-hash) vocab
    (slot-value (gethash word word-hash) 'code)))

(defmethod get-indx-word ((vocab w2v-vocab) indx)
  (with-slots (indx-hash) vocab
    (gethash indx indx-hash)))

(defmethod add-word-safe ((vocab w2v-vocab) word freq)
  (with-slots (word-hash indx-hash vocab-size) vocab
    (setf (gethash word word-hash) (make-instance 'w2v-word-info :freq freq
                                                  :indx vocab-size))
    (setf (gethash vocab-size indx-hash) word)
    (incf vocab-size)))

(defmethod incf-word-freq ((vocab w2v-vocab) word freq)
  (with-slots (word-hash) vocab
    (incf-freq (gethash word word-hash) freq)))

(defmethod add-word-to-vocab ((vocab w2v-vocab) word &optional (freq 1))
  (if (has-word-in-vocab vocab word)
      (incf-word-freq vocab word freq)
      (add-word-safe vocab word freq)))

(defmethod sorted-vocab ((vocab w2v-vocab) &key (compare #'>) (filter :value))
  (with-slots (word-hash) vocab
    (let ((vocab-lst nil))
      (labels ((collect (word info)
                 (ecase filter
                   (:full (push (list word info) vocab-lst))
                   (:key (push (list word info) vocab-lst))
                   (:value (push info vocab-lst)))))
        (maphash #'collect word-hash))
      (ecase filter
        (:full (sort vocab-lst compare :key (lambda (info) (winfo-freq (second info)))))
        (:key (mapcar #'car (sort vocab-lst compare :key (lambda (info)
                                                           (winfo-freq (second info))))))
        (:value (sort vocab-lst compare :key (lambda (info) (winfo-freq info))))))))

(defmethod create-huffman ((vocab w2v-vocab))
  (let ((winfos (sorted-vocab vocab :compare #'< :filter :value)))
    (multiple-value-bind (huff coding parents binarys)
        (huffman-from-list winfos :coded t :keyfn #'identity :freqfn #'winfo-freq :sorted t)
      (declare (ignore huff coding))
      (let ((path nil)
            (nleaf (length winfos)))
        (dotimes (i nleaf)
          (setf path nil)
          ;; `indx' is index of inner node, which should subtract `nleaf'
          (do ((indx (nth i parents) (nth indx parents)))
              ((null indx))
            (push (- indx nleaf) path))
          (fill-huffman (nth i winfos) path (nth i binarys)))))))

(defun learn-vocab-from-memory (fpath &key (eow #\Space))
  "small file which fits into memory"
  (let ((vocab (make-instance 'w2v-vocab)))
    (do-example-from-file (example indx fpath)
      (let ((words (split-string example eow)))
        (dolist (word words)
          (add-word-to-vocab vocab word))))
    vocab))

(defun learn-vocab-from-file (fpath &key (eow #\Space) (eol #\Newline))
  (let ((vocab (make-instance 'w2v-vocab))
        (fs (open fpath)))
    (do-word (word fs :eow eow :eol eol)
      (unless (eq word :eol)
        (add-word-to-vocab vocab word)))
    (close fs)
    vocab))

(defun learn-vocab-from-string (string &key (eow #\Space) (eol #\Newline))
  (let ((vocab (make-instance 'w2v-vocab))
        (sents (split-string string eol))
        (words nil))
    (dolist (sent sents)
      (setf words (split-string sent eow))
      (dolist (word words)
        (add-word-to-vocab vocab word)))
    vocab))

(defun learn-vocab-from-list (lsts)
  "`lsts' being list of lists"
  (let ((vocab (make-instance 'w2v-vocab)))
    (dolist (sent lsts)
      (dolist (word sent)
        (add-word-to-vocab vocab word)))
    vocab))

(defun get-next-sentence (iterable i &key (max-len 100) (eow #\Space) (eol #\Newline))
  "get (i+1)-th sentence from stream, or list
Returns: :eof if no more sentence; or list of words (could be empty list)"
  (etypecase iterable
    (stream (read-next-sentence iterable :max-len max-len :eow eow :eol eol))
    (list (if (>= i (length iterable))
              :eof
              (nth (+ i 1) iterable)))
    (t (error "get-next-sentence only supports stream or list"))))
