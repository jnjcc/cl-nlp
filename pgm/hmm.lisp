;;;; Copyright (c) 2012-2015 jnjcc, Yste.org. All rights reserved.
;;;;
;;;; (First-order) Hidden Markov Model $\lambda = (S, V, \pi, A, B)$:
;;;;   - The Evaluation Problem and the Forward / Backward Algorithm
;;;;   - The Decoding Problem and the Viterbi Algorithm
;;;;   - The Learning Problem and the MLE / Baum-Welch Algorithm

(in-package #:cl-nlp)

;;; states: a set of hidden states
;;;   - stateseq: state sequence, (state state ...)
;;;   - statecorpus: a collection of state sequence, (stateseq stateseq ...)
;;; observation symbols: a set of symbols
;;;   - obsseq: observation sequence, (symbol symbol ...)
;;;   - obscorpus: a collection of observation sequence, (obsseq obsseq ...)
;;;   - compseq: complete-data, observation with associated state, ((symbol . state) ...)
;;;   - compcorpus: complete-data, (compseq compseq ...)
(defclass hmm-model ()
  ((testfn :initform 'equal :initarg :test :documentation "hash key test function")
   (states :initform nil :documentation "HMM states S as a list")
   (symbls :initform nil :documentation "HMM symbols V as a list")
   (startp :initform nil :documentation "HMM start probability \pi")
   (transp :initform nil :documentation "HMM transition probability A")
   (emissp :initform nil :documentation "HMM emission probability B")))

(defmethod reset-states ((hmm hmm-model))
  (with-slots (testfn states symbls startp transp emissp) hmm
    (setf states (make-freq-dist :hash :test testfn))
    (setf symbls (make-freq-dist :hash :test testfn))
    (setf startp (make-freq-dist :hash :test testfn))
    (setf transp (make-cond-freq :test testfn))
    (setf emissp (make-cond-freq :test testfn))))

(defmethod hmm-states ((hmm hmm-model))
  (with-slots (states) hmm
    states))

(defmethod hmm-symbols ((hmm hmm-model))
  (with-slots (symbls) hmm
    symbls))

(defmethod hmm-start-prob ((hmm hmm-model) state)
  (with-slots (startp) hmm
    (pdist-prob startp state)))

(defmethod hmm-start-probi ((hmm hmm-model) is)
  "start probability for `is'-th state"
  (with-slots (states) hmm
    (hmm-start-prob hmm (nth is states))))

(defmethod hmm-trans-prob ((hmm hmm-model) state1 state2)
  (with-slots (transp) hmm
    (pdist-cond-prob transp state1 state2)))

(defmethod hmm-trans-probi ((hmm hmm-model) is1 is2)
  "transition probability from `is1'-th state to `is2'-th state"
  (with-slots (states) hmm
    (hmm-trans-prob hmm (nth is1 states) (nth is2 states))))

(defmethod hmm-emiss-prob ((hmm hmm-model) state symbl)
  (with-slots (emissp) hmm
    (pdist-cond-prob emissp state symbl)))

(defmethod hmm-emiss-probi ((hmm hmm-model) is symbl)
  (with-slots (states) hmm
    (hmm-emiss-prob hmm (nth is states) symbl)))

;;; The Evaluation Problem and the Forward / Backward Algorithm
(defmethod hmm-forward-alpha ((hmm hmm-model) obsseq)
  "T by N array of probabilities, with T being length of the sequence and N being
number of states. `alpha[t][s]': the probability of being in state s at time t after
observing the partial symbol sequence up to and including t"
  (with-slots (states symbls) hmm
    (let* ((symT (length obsseq))
           (statN (length states))
           (alpha (make-array `(,symT ,statN) :initial-element 0)))
      (let ((symbl (nth 0 obsseq)))
        (dotimes (i statN)
          (setf (aref alpha 0 i)
                (* (hmm-start-probi hmm i)
                   (hmm-emiss-probi hmm i symbl)))))

      (loop for ts from 1 below symT do
           (dotimes (i statN)
             (dotimes (j statN)
               (incf (aref alpha ts i) (* (aref alpha (- ts 1) j)
                                          (hmm-trans-probi hmm j i)
                                          (hmm-emiss-probi hmm i (nth ts obsseq)))))))
      alpha)))

(defmethod hmm-forward ((hmm hmm-model) obsseq)
  (with-slots (states symbls) hmm
    (let ((prob 0.0)
          (symT (length obsseq))
          (statN (length states))
          (alpha (hmm-forward-alpha hmm obsseq)))
      (dotimes (i statN)
        (incf prob (aref alpha (- symT 1) i)))
      (values prob alpha))))

(defmethod hmm-backward-beta ((hmm hmm-model) obsseq)
  "T by N array of probabilities. `beta[t][s]': the probability of being in state s
at time t after observing the partial symbol sequence from t ... T"
  (with-slots (states symbls) hmm
    (let* ((symT (length obsseq))
           (statN (length states))
           (beta (make-array `(,symT ,statN) :initial-element 0)))
      (dotimes (i statN)
        (setf (aref beta (- symT 1) i) 1))

      (loop for ts from (- symT 2) downto 0 do
           (dotimes (i statN)
             (dotimes (j statN)
               (incf (aref beta ts i) (* (aref beta (+ ts 1) j)
                                         (hmm-trans-probi hmm i j)
                                         (hmm-emiss-probi hmm j (nth (+ ts 1) obsseq)))))))
      beta)))

(defmethod hmm-backward ((hmm hmm-model) obsseq)
  "T by N array of probabilities. `beta[t][s]': the probability of being in state s
at time t after observing the partial symbol sequence from t ... T"
  (with-slots (states symbls) hmm
    (let ((statN (length states))
          (beta (hmm-backward-beta hmm obsseq))
          (prob 0.0))
      (let ((symbl (nth 0 obsseq)))
        (dotimes (i statN)
          (incf prob (* (hmm-start-probi hmm i)
                        (hmm-emiss-probi hmm i symbl)
                        (aref beta 0 i)))))
      (values prob beta))))

;;; The Decoding Problem and the Viterbi Problem
(defmethod %fill-symbol-index ((hmm hmm-model) obsseq)
  (with-slots (symbls) hmm
    (let ((symidx (make-hash-table :test 'equal)))
      (let ((idx 0))
        (dolist (symbl obsseq)
          (setf (gethash symbl symidx) idx)
          (incf idx))
        (dolist (symbl obsseq)
          ;; we know for sure `nil' means not exist in this context
          (unless (gethash symbl symidx)
            (setf (gethash symbl symidx) idx)
            (incf idx))))
      symidx)))

(defmethod hmm-decode ((hmm hmm-model) obsseq)
  "decode hidden states for symbol sequence `obsseq' using Viterbi algorithm"
  (with-slots (states symbls) hmm
    (let* (;; length of test `obsseq'
           (symT (length obsseq))
           ;; number of states
           (statN (length states))
           ;; records max probability leading to each possible state at time step t
           (delta (make-array `(,symT ,statN) :initial-element 0))
           ;; backtrace
           (backtrace (make-array `(,symT ,statN) :initial-element -1)))
      (let ((symbl (nth 0 obsseq)))
        (dotimes (i statN)
          (setf (aref delta 0 i) ;; delta[1][i]
                (logprod (hmm-start-probi hmm i)
                         (hmm-emiss-probi hmm i symbl)))))
      ;; t = 2, 3, ..., T
      (loop for ts from 1 below symT do
           (dotimes (i statN)
             (let (best argbest curr)
               (dotimes (j statN)
                 (setf curr (rlogprod (aref delta (- ts 1) j) (hmm-trans-probi hmm j i)))
                 (when (or (null best) (> curr best))
                   (setf best curr)
                   (setf argbest j)))
               (setf (aref delta ts i) (rlogprod best (hmm-emiss-probi hmm i (nth ts obsseq))))
               (setf (aref backtrace ts i) argbest))))

      (let (best argbest curr tags)
        (dotimes (i statN)
          (setf curr (aref delta (- symT 1) i))
          (when (or (null best) (> curr best))
            (setf best curr)
            (setf argbest i)))
        (push (cons (car (last obsseq)) (nth argbest states)) tags)
        (loop for ts from (- symT 1) downto 1 do
             (setf argbest (aref backtrace ts argbest))
             (push (cons (nth (- ts 1) obsseq) (nth argbest states)) tags))
        tags))))

;;; The Learning Problem - 1:
;;;   HMM model from probability literal
(defmethod hmm-literal ((hmm hmm-model) Slst Vlst pilst Amat Bmat)
  (with-slots (states symbls startp transp emissp) hmm
    (setf states Slst)
    (setf symbls Vlst)
    (setf startp (derive-literal states pilst))
    (setf transp (derive-cond-literal states states Amat))
    (setf emissp (derive-cond-literal states symbls Bmat))))

;;; The Learning Problem - 2:
;;;   MLE from complete corpus
(defmethod hmm-learn ((hmm hmm-model) compcorpus &optional etype)
  (reset-states hmm)
  (with-slots (states symbls startp transp emissp) hmm
    (unless etype
      (setf etype :mle))
    (dolist (compseq compcorpus)
      (let ((pre-stat nil)
            symb stat)
        (dolist (pair compseq)
          (setf symb (car pair))
          (fdist-incf symbls symb)
          (setf stat (cdr pair))
          (fdist-incf states stat)
          (if pre-stat
              (fdist-cond-incf transp pre-stat stat)
              (fdist-incf startp stat))
          (fdist-cond-incf emissp stat symb)
          (setf pre-stat stat))))
    (setf states (fdist-keys states))
    (setf symbls (fdist-keys symbls))
    (setf startp (derive-prob startp (length states) etype))
    (setf transp (derive-cond-prob transp (length states) etype))
    (setf emissp (derive-cond-prob emissp (length symbls) etype))))

;;; The Learning Problem - 3:
;;;   Baum-Welch algorithm from incomplete corpus
(defmethod hmm-init-random ((hmm hmm-model) stats syms)
  (let* ((statN (length stats))
         (symM (length syms))
         (pilst (randprob1d statN))
         (Amat (randprob2d statN statN))
         (Bmat (randprob2d statN symM)))
    (hmm-literal hmm stats syms pilst Amat Bmat)))

(defmethod hmm-fb-arrays ((hmm hmm-model) obsseq)
  "forward and backward arrays"
  (multiple-value-bind (fzprob alpha) (hmm-forward hmm obsseq)
    (multiple-value-bind (bzprob beta) (hmm-backward hmm obsseq)
      (values fzprob alpha beta bzprob))))

(defmethod hmm-bw-arrays ((hmm hmm-model) symT statN obsseq alpha beta zprob)
  "gamma[t][i]: P(s[t] = i);
xi[t][i][j]: P(s[t+1] = j | s[t] = i)"
  (let ((gamma (make-array (list symT statN) :initial-element 0.0))
        (xi (make-array (list (- symT 1) statN statN) :initial-element 0.0)))
    (dotimes (ts symT)
      (dotimes (i statN)
        (setf (aref gamma ts i) (/ (* (aref alpha ts i) (aref beta ts i)) zprob))
        (unless (= ts (- symT 1))
          (dotimes (j statN)
            (setf (aref xi ts i j) (/ (* (aref alpha ts i) (hmm-trans-probi hmm i j)
                                         (hmm-emiss-probi hmm j (nth (+ ts 1) obsseq))
                                         (aref beta (+ ts 1) j))
                                      zprob))))))
    (values gamma xi)))

(defmethod hmm-baum-welch ((hmm hmm-model) incomplete)
  (with-slots (states symbls startp transp emissp) hmm
    (let ((statN (length states))
          (symT nil))
      (dolist (obsseq incomplete)
        (setf symT (length obsseq))
        (multiple-value-bind (fzprob alpha beta bzprob) (hmm-fb-arrays hmm obsseq)
          (unless (float= fzprob bzprob)
            (format t "forward probability != backward probability~%"))
          (multiple-value-bind (gamma xi) (hmm-bw-arrays hmm symT statN obsseq alpha beta fzprob)
            (dotimes (i statN)
              (pdist-setf startp (nth i states) (aref gamma 0 i))
              (let ((numer 0.0)
                    (denom 0.0))
                (dotimes (j statN)
                  (setf numer 0.0)
                  (setf denom 0.0)
                  (dotimes (ts (- symT 1))
                    (incf numer (aref xi ts i j))
                    (incf denom (aref gamma ts i)))
                  (pdist-cond-setf transp (nth i states) (nth j states) (/ numer denom)))

                (dotimes (k (length symbls))
                  (setf numer 0.0)
                  (setf denom 0.0)
                  (dotimes (ts symT)
                    (incf denom (aref gamma ts i))
                    (when (equal (nth ts obsseq) (nth k symbls))
                      (incf numer (aref gamma ts i))))
                  (pdist-cond-setf emissp (nth i states) (nth k symbls) (/ numer denom)))))))))))

(defmethod hmm-learn-bw ((hmm hmm-model) stats symbs incomplete &optional (epochs 10))
  (hmm-init-random hmm stats symbs)
  (dotimes (ep epochs)
    (hmm-baum-welch hmm incomplete)))
