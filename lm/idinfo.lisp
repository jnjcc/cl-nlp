;;;; Copyright (c) 2012-2015 jnjcc, Yste.org. All rights reserved.
;;;;
;;;; data structures for idngram info

(in-package #:cl-nlp)

;;; Let m <= n be any gram, `freq' be frequency of some m-gram, `idx' be index of some m-gram
;;;   - (idngram-tree)
;;;     + `mgram-fof'[m][freq]: freq of freq table for each m-gram, and each `freq'
;;;     + `mgram-nums'[m]: # of grams for each m-gram (as for unigram, excluding <UNK>)
;;;     + `mgram-freqs'[m][idx]: gram freq for each `idx'-th m-gram (in numeric sorted order)
;;;     + `mgram-wdids'[m][idx]: for `idx'-th m-gram, what is the end wordid?
;;;        - e.g., for '1 2 3' and '1 3 1', 1-th bigram's end wordid is 2, 2-th bigram's end wordid is 3
;;;     + `mgram-alphas'[m][idx]: for `idx'-th m-gram, what is the backoff alpha? (Katz Backoff)
;;;        - default 1.0, will change later
;;;     + `mgram-ptrs'[m][idx]: for `idx'-th m-gram, what is the latest (m+1)-gram it points to?
;;;        - e.g., for 'A B C' and 'A B D', 1-th bigram 'A B' points to the 2-th trigram 'A B D'
;;;   - (discount-ratio) & (discount-unigram)
;;;     + `mgram-disc'[m][freq]: discount ratio for `freq' of m-gram
;;;     + `ugram-prob'[idx]: discounted probability for each unigram
;;;     + `ugram-logp'[idx]: log probability for each unigram
(defclass idinfo-type ()
  ((n :initform 3 :initarg :n :reader idm-get-n)
   (disc-range :initform *fof-disc-range* :initarg :disc-range)
   (uni-range :initform *fof-uni-range* :initarg :uni-range)
   (mgram-fof :initform nil :reader mgram-fof)
   (mgram-nums :initform nil :reader mgram-nums)
   (mgram-freqs :initform nil :reader mgram-freqs)
   (mgram-wdids :initform nil :reader mgram-wdids)
   (mgram-alphas :initform nil :reader mgram-alphas)
   (mgram-ptrs :initform nil :reader mgram-ptrs)

   (discount-type :initform :good-turing :reader discount-type)
   (mgram-disc :initform nil :reader mgram-disc)
   (ugram-prob :initform nil :reader ugram-prob)
   (ugram-logp :initform nil :reader ugram-logp)))

(defmethod initialize-instance :after ((idm idinfo-type) &rest args)
  (declare (ignore args))
  (with-slots (n disc-range uni-range) idm
    (with-slots (mgram-fof mgram-nums mgram-freqs mgram-wdids mgram-alphas mgram-ptrs mgram-disc) idm
      (setf mgram-fof (fof-init n disc-range uni-range))
      (setf mgram-nums (mlist-init n :initial-element 0))
      (setf mgram-freqs (lol-init n))
      (setf mgram-wdids (lol-init n))
      (setf mgram-alphas (lol-init n))
      (setf mgram-ptrs (lol-init n))

      (setf mgram-disc (fof-copy-init mgram-fof)))))

(defmethod idm-get-fof ((idm idinfo-type) m freq)
  (with-slots (mgram-fof) idm
    (fof-ref mgram-fof m freq)))

(defmethod idm-get-gnum ((idm idinfo-type) m)
  (with-slots (mgram-nums) idm
    (mlist-ref mgram-nums m)))

(defmethod idm-get-freq ((idm idinfo-type) m idx)
  (with-slots (mgram-freqs) idm
    (lol-ref mgram-freqs m idx)))

(defmethod idm-get-wdid ((idm idinfo-type) m idx)
  (with-slots (mgram-wdids) idm
    (lol-ref mgram-wdids m idx)))

(defmethod idm-get-alpha ((idm idinfo-type) m idx)
  (with-slots (mgram-alphas) idm
    (lol-ref mgram-alphas m idx)))

(defmethod idm-get-ptrs ((idm idinfo-type) m idx)
  (with-slots (mgram-ptrs) idm
    (lol-ref mgram-ptrs m idx)))

(defmethod idm-get-unigram ((idm idinfo-type))
  (with-slots (mgram-freqs) idm
    (lol-get mgram-freqs 1)))

(defmethod idm-get-uprob ((idm idinfo-type) idx)
  (with-slots (ugram-prob) idm
    (mlist-ref ugram-prob idx)))

(defmethod idm-get-logpr ((idm idinfo-type) idx)
  (with-slots (ugram-logp) idm
    (mlist-ref ugram-logp idx)))

(defmethod idm-incf-fof ((idm idinfo-type) m freq &optional (delta 1))
  (with-slots (mgram-fof) idm
    (fof-incf mgram-fof m freq delta)))

(defmethod idm-incf-gnum ((idm idinfo-type) m &optional (delta 1))
  "gram number"
  (with-slots (mgram-nums) idm
    (mlist-incf mgram-nums m delta)))

(defmethod idm-aset-gram ((idm idinfo-type) m freq wdid &optional (alpha 1.0))
  "append and set `freq', `wdid' and `alpha' for a brand new m-gram"
  (with-slots (mgram-nums mgram-freqs mgram-wdids mgram-alphas) idm
    (let ((idx (mlist-ref mgram-nums m)))
      (lol-aset mgram-freqs m idx freq)
      (lol-aset mgram-wdids m idx wdid)
      (lol-aset mgram-alphas m idx alpha))))

(defmethod idm-aset-ptrs ((idm idinfo-type) m)
  "remember current m-gram points to current (m+1)-gram"
  (with-slots (mgram-nums mgram-ptrs) idm
    (lol-aset mgram-ptrs m (mlist-ref mgram-nums m) (mlist-ref mgram-nums (+ m 1)))))

(defmethod idm-setf-alpha ((idm idinfo-type) m idx val)
  (with-slots (mgram-alphas) idm
    (lol-setf mgram-alphas m idx val)))
