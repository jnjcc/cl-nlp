;;;; Copyright (c) 2012-2015 jnjcc, Yste.org. All rights reserved.
;;;;

(in-package #:cl-nlp)

(defclass node2vec ()
  ((embed-size :initform 128 :initarg :embed-size :documentation "embedding size")
   (w2v-model :initform nil :documentation "word2vec model")
   (walk-length :initform 80 :initarg :walk-length :documentation "random walk steps")
   (num-walks :initform 10 :initarg :num-walks :documentation "number of walks per src,
thus the total number of walks is `num-walks' * `node-count'")
   (return-p :initform 1 :initarg :return-p :documentation "parameter p controls
the likelihood of immediately revisiting a node in the walk.")
   (inout-q :initform 1 :initarg :inout-q :documentation "parameter q allows the
search to differentiate between 'inward' and 'outward' nodes")
   (alias-nodes :initform nil :documentation "init alias sampling for nodes")
   (alias-edges :initform nil :documentation "init alias sampling for edges")))

(defun %alias-from-weights (weights)
  (multiple-value-bind (ptable atable) (alias-initialize weights)
    (list ptable atable)))

(defmethod alias-of-edge ((n2v node2vec) X src dst weight)
  "the core transition probability of node2vec"
  (with-slots (return-p inout-q) n2v
    (let ((neighbors (sorted-neighbors X dst))
          (dst-nbr nil)
          (dst-wei nil)
          (weights nil))
      (dolist (dst-lst neighbors)
        (setf dst-nbr (first dst-lst))
        (setf dst-wei (second dst-lst))
        (cond
          ;; trying to return to `src': [weight / p]
          ((same-node X dst-nbr src) (push (/ dst-wei return-p) weights))
          ;; trying to go to neighbors of `dst'; edge exists between neighbor and `src'
          ((has-edge X dst-nbr src) (push dst-wei weights))
          ;; trying to go to neighbors of `dst'; no edge exists between neighbor and `src'
          (t (push (/ dst-wei inout-q) weights))))
      (setf weights (nreverse weights))
      (%alias-from-weights weights))))

(defmethod alias-of-node ((n2v node2vec) X node value)
  (declare (ignore value))
  (let ((neighbors (sorted-neighbors X node))
        (weights nil))
    (setf weights (mapcar #'second neighbors))
    (%alias-from-weights weights)))

(defmethod init-trans-probs ((n2v node2vec) X)
  "probabilities of node and edge for random walk"
  (with-slots (alias-nodes alias-edges) n2v
    (setf alias-nodes (make-hash-table :test (cl-ml/graph::node-eq X)))
    (setf alias-edges (make-hash-table :test 'equal))
    (do-graph-nodes (node value X)
      (setf (gethash node alias-nodes) (alias-of-node n2v X node value)))
    (ecase (graph-type X)
      (:undirected
       (do-graph-edges (un vn weight X)
         (setf (gethash (list un vn) alias-edges) (alias-of-edge n2v X un vn weight))
         (setf (gethash (list vn un) alias-edges) (alias-of-edge n2v X vn un weight))))
      (:directed
       (do-graph-edges (un vn weight X)
         (setf (gethash (list un vn) alias-edges) (alias-of-edge n2v X un vn weight)))))))

(defmethod simulate-one-walk ((n2v node2vec) X src)
  "Requires: (init-trans-probs) called!"
  (with-slots (walk-length alias-nodes alias-edges) n2v
    (let ((walk-lst (list src))
          ;; edge of (`prenode', `curnode')
          (prenode nil)
          (curnode nil)
          ;; alias tables of `curnode', or of the edge (`prenode', `curnode')
          (ctables nil)
          ;; neighbors of `curnode'
          (curnbrs nil))
      (dotimes (step (- walk-length 1))
        (setf curnode (car walk-lst))
        ;; we do not need edge weight now
        (setf curnbrs (sorted-neighbors X curnode :weighted nil))
        (when (null curnbrs)
          (return))
        (if (= (length walk-lst) 1)
            (setf ctables (gethash curnode alias-nodes))
            (progn
              (setf prenode (second walk-lst))
              (setf ctables (gethash (list prenode curnode) alias-edges))))
        (push (alias-draw-element curnbrs (first ctables) (second ctables)) walk-lst))
      (nreverse walk-lst))))

(defmethod simulate-walks ((n2v node2vec) X)
  "Repeatedly simulate random walks from each node
Requires: (init-trans-probs) called!"
  (with-slots (num-walks) n2v
    (let ((walks nil)
          (nodes (get-nodes X :valued nil)))
      (dotimes (i num-walks)
        (shuffle nodes)
        (dolist (src nodes)
          (push (simulate-one-walk n2v X src) walks)))
      walks)))

(defmethod fit ((n2v node2vec) X &optional y)
  "X as a graph"
  (declare (ignore y))
  (init-trans-probs n2v X)
  (with-slots (embed-size w2v-model) n2v
    (setf w2v-model (make-instance 'word2vec :dimensions embed-size))
    (let ((walks (simulate-walks n2v X)))
      (fit-from-list w2v-model walks))))

(defmethod transform ((n2v node2vec) node)
  (with-slots (w2v-model) n2v
    (transform w2v-model node)))
