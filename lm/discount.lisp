;;;; Copyright (c) 2012-2015 jnjcc, Yste.org. All rights reserved.
;;;;
;;;; Discounting is a kind of smoothing algorithm

(in-package #:cl-nlp)

(defun %good-turing (mgram-fof m disc-ratios)
  "Good-Turing discounting ratio in Katz backoff model"
  (let* ((disc (fof-disc mgram-fof m))
         (k disc)
         ;; common term for Katz's back-off model
         (common nil)
         (vanilla nil)
         (r nil))
    (when (and (= m 1) (= disc 1) (> (fof-ref mgram-fof m 0) 0))
      ;; special treatment for unigram
      (fof-setf disc-ratios m 1 (/ (fof-ref mgram-fof m 1)
                                   (+ (fof-ref mgram-fof m 1) (fof-ref mgram-fof m 0))))
      (return-from %good-turing))
    ;; (k+1)*n[k+1] / n[1]
    (setf common (/ (* (+ k 1) (fof-ref mgram-fof m (+ k 1))) (fof-ref mgram-fof m 1)))
    (when (or (<= common 0.0) (>= common 1.0))
      (return-from %good-turing))
    (dotimes (i k)
      (setf r (+ i 1))
      (setf vanilla 1.0)
      (unless (= (fof-ref mgram-fof m r) 0)
        ;; vanilla Good-Turing count: rstar = (r+1) * (n[r+1] / n[r])
        (setf vanilla (/ (* (+ r 1) (fof-ref mgram-fof m (+ r 1))) (fof-ref mgram-fof m r)))
        ;; vanilla Good-Turing ratio: (rstar / r)
        (setf vanilla (/ vanilla r)))
      ;; NOTICE: the default discount ratio is 1.0
      ;; TODO: when 0.0 happens, we might need to decrease `disc-range'
      (unless (or (<= vanilla 0.0) (>= vanilla 1.0)  (<= vanilla common))
        (fof-setf disc-ratios m r (/ (- vanilla common) (- 1.0 common)))))))

(defun %discount-ratio (mgram-fof n disc-type mgram-disc)
  (let ((m 1))
    (dotimes (i n)
      (setf m (+ i 1))
      (ecase disc-type
        (:good-turing (%good-turing mgram-fof m mgram-disc))))))

(defun discount-ratio (mgram-fof n disc-type)
  "discounting m-gram within discount range"
  (let ((mgram-disc (fof-copy-init mgram-fof)))
    (%discount-ratio mgram-fof n disc-type mgram-disc)
    mgram-disc))

(defun discount-freq (mgfreq m disc-ratio disc-type)
  "Given `disc-ratio', return discounted `mgfreq' of m-gram"
  (ecase disc-type
    (:good-turing
     (when (<= mgfreq (fof-disc disc-ratio m))
       (setf mgfreq (* (fof-ref disc-ratio m mgfreq) mgfreq)))))
  mgfreq)

(defun discount-unigram (unigram idmin idmax num-zeros disc-ratio disc-type &optional (zero-frac 1.0))
  "discount unigram distribution, give some mass to zerotons:
`unigram' being frequency of each unigram, including <UNK>, etc;
`num-zeros' being number of zerotons, <UNK> being one of them;
`disc-type' being discounting type: :good-turing etc
`zero-frac': P(zeroton) is not allowed to exceed `zero-frac' * P(singleton)"
  (let ((uni (fof-disc disc-ratio 1))
        (sumfreq (reduce #'+ unigram :initial-value 0.0))
        (probs (mlist-copy unigram))
        (logprobs (mlist-copy unigram))
        curfreq curprob totprob leftprob
        zeroprob singprob)
    (setf totprob 0.0)
    (loop for id from idmin to idmax do
         (setf curfreq (mlist-ref unigram id))
         (setf curprob (/ curfreq sumfreq))
         (ecase disc-type
           (:good-turing
            (cond
              ((<= 1 curfreq uni) (setf curprob (* curprob (fof-ref disc-ratio 1 curfreq))))
              ((= 0 curfreq) (setf curprob 1e-99)))))
         (mlist-setf probs id curprob)
         (incf totprob curprob))
    ;; (1.0 - totprob) probability leftover for zerotons
    (setf leftprob (- 1.0 totprob))
    (setf zeroprob (/ leftprob num-zeros))
    ;; probability for singleton (words that occurred exactly once in training data)
    ;;   we might have to make sure probability of zerotons does not exceed this
    (setf singprob (/ 1.0 sumfreq))
    (ecase disc-type
      (:good-turing
       (when (>= uni 1)
         (setf singprob (* singprob (fof-ref disc-ratio 1 1))))))
    (when (> zeroprob (* zero-frac singprob))
      (setf zeroprob (* zero-frac singprob)))
    (loop for id from idmin to idmax do
         (setf curfreq (mlist-ref unigram id))
         (when (= curfreq 0)
           (mlist-setf probs id zeroprob)))
    ;; probability of zeroton might have changed by `zero-frac'
    (setf leftprob (- leftprob (* zeroprob num-zeros)))
    (when (> leftprob 0)
      (loop for id from idmin to idmax do
           (let ((old (mlist-ref probs id)))
             (mlist-setf probs id (/ old (- 1.0 leftprob))))))
    (loop for id from idmin to idmax do
         (let ((pr (mlist-ref probs id)))
           (mlist-setf logprobs id (log pr))))
    (values probs logprobs)))

(defmethod idm-discount-ratio ((idm idinfo-type) disc-type)
  (with-slots (n mgram-fof discount-type mgram-disc) idm
    (setf discount-type disc-type)
    (%discount-ratio mgram-fof n disc-type mgram-disc)))

(defmethod idm-discount-freq ((idm idinfo-type) m freq)
  (with-slots (discount-type mgram-disc) idm
    (discount-freq freq m mgram-disc discount-type)))

(defmethod idm-discount-ugram ((idm idinfo-type) idmin idmax &optional (zero-frac 1.0))
  "`zero-frac': P(zeroton) is not allowed to exceed `zero-frac' * P(singleton)"
  (with-slots (discount-type mgram-disc ugram-prob ugram-logp) idm
    (multiple-value-setq (ugram-prob ugram-logp)
      (discount-unigram (idm-get-unigram idm) idmin idmax (idm-get-fof idm 1 0)
                        mgram-disc discount-type zero-frac))))
