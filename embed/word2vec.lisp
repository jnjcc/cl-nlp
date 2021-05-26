;;;; Copyright (c) 2012-2015 jnjcc, Yste.org. All rights reserved.
;;;;

(in-package #:cl-nlp)

(defclass word2vec ()
  ((dimensions :initform 128 :initarg :dimensions
               :documentation "dimensionality of the word vectors")
   (vocabulary :initform nil :documentation "word2vec vocabulary")
   (in-matrix :initform nil :documentation "input vectors of words")
   (out-matrix :initform nil :documentation "output vectors of words")
   (window :initform 5 :initarg :window
           :documentation "maximum distance between center and context word")
   (min-count :initform 5 :initarg :min-count
              :documentation "discard words with frequency less than this")
   (max-sents :initform 100 :initarg :max-sents
              :documentation "max length of training sentences")
   (algo :initform :skip-gram :initarg :algo
         :documentation "training algorithm: :skip-gram or :cbow")
   (eta0 :initform 0.025 :initarg :eta0
         :documentation "the starting learning rate, default is 0.025 for :skip-gram
and 0.05 for :cbow")
   (eta :initform nil :documentation "learn rate schedule")
   (hsoftmax :initform nil :initarg :hsoftmax
             :documentation "hierarchical softmax, t or nil")
   (negsample :initform 5 :initarg :negsample
              :documentation "number of noise words in negative sampling,
common values are [3, 10]")
   (negtable :initform nil :documentation "sampling table for negative sampling")
   (epochs :initform 100 :initarg :epochs
           :documentation "number of epochs over the corpus")))

(defmethod %widx ((w2v word2vec) word)
  "word string -> matrix index"
  (with-slots (vocabulary) w2v
    (get-word-indx vocabulary word)))

(defmethod %winv ((w2v word2vec) word)
  "input vector for word"
  (with-slots (in-matrix) w2v
    (mrv in-matrix (%widx w2v word))))

(defmethod %winvi ((w2v word2vec) i)
  "input vector for i-th word"
  (with-slots (in-matrix) w2v
    (mrv in-matrix i)))

(defmethod %woutv ((w2v word2vec) word)
  "output vector for word"
  (with-slots (out-matrix) w2v
    (mcv out-matrix (%widx w2v word))))

(defmethod %woutvi ((w2v word2vec) i)
  "output vector for i-th word"
  (with-slots (out-matrix) w2v
    (mcv out-matrix i)))

(defmethod %word ((w2v word2vec) indx)
  "matrix index -> word string"
  (with-slots (vocabulary) w2v
    (get-indx-word vocabulary indx)))

(defmethod initialize-instance :after ((w2v word2vec) &rest args)
  (declare (ignore args))
  (with-slots (eta0 eta) w2v
    (setf eta eta0)))

(defmethod learn-rate-schedule ((w2v word2vec) nwords)
  (with-slots (vocabulary eta0 eta) w2v
    (let ((vocsz (vocab-size vocabulary)))
      (setf eta (* eta0 (- 1 (/ nwords (+ vocsz 1)))))
      (when (< eta (* eta0 0.0001))
        (setf eta (* eta0 0.0001))))))

(defmethod hierarchal-softmax ((w2v word2vec) sentence center randwin hiddens hidgrad)
  ":cbow, `hiddens' is average of context words, `center' is target word;
:skip-gram, `hiddens' is of context word (which is a little different from papers), while
`center' is still target word"
  (with-slots (vocabulary algo eta) w2v
    (create-huffman vocabulary)
    ;; (yhat - label) * vj, where vj is inner node of huffman tree
    ;; `hpath': huffman path for `center' word, contains inner node index into `out-matrix'
    (let (yhat label vj hpath)
      ;; left child: label 1; right child: label 0
      (setf label (- 1 (get-word-code vocabulary (nth center sentence))))
      (setf hpath (get-word-path vocabulary (nth center sentence)))
      (dolist (nidx hpath)
        (setf vj (%woutvi w2v nidx))
        (setf yhat (cl-ml::sigmoid (vdot vj hiddens)))
        ;; eta * (yhat - label)
        (setf yhat (* eta (- yhat label)))
        (do-vector (i hidgrad)
          (incf (vref hidgrad i) (* yhat (vref vj i))))
        (do-vector (i vj)
          (decf (vref vj i) (* yhat (vref hiddens i)))))
      (ecase algo
        (:cbow
         ;; update gradient for each context word
         (do ((context (- center randwin) (1+ context)))
             ((> context (+ center randwin)))
           (when (and (< -1 context (length sentence))
                      (/= context center))
             (setf hiddens (%winv w2v (nth context sentence)))
             (do-vector (i hiddens)
               ;; maybe we should multiply 1/cw on gradient
               (decf (vref hiddens i) (vref hidgrad i))))))
        (:skip-gram
         ;; with :skip-gram, `hiddens' is just alias of `context' word
         (do-vector (i hiddens)
           (decf (vref hiddens i) (vref hidgrad i))))))))

(defmethod subsmpling-word ((w2v word2vec))
  "The subsampling randomly discards frequent words while keeping the ranking same"
  )

;;; TODO: negative sampling
(defmethod %init-negative-sampling ((w2v word2vec))
  )

(defmethod neg-sampling-word ((w2v word2vec))
  (with-slots (in-matrix) w2v
    (random (nrow in-matrix))))

(defmethod negative-sampling ((w2v word2vec) sentence center randwin hiddens hidgrad)
  "(`center', context) is a training sample; with `hiddens' being input vector of `context'!!!"
  (with-slots (algo eta negsample) w2v
    ;; (yhat - label) * vj, where vj is negative words, or `center'
    ;; `cidx': index of `center' into `out-matrix'
    (let (yhat label vj cidx)
      (setf cidx (%widx w2v (nth center sentence)))
      (dotimes (neg (+ negsample 1))
        (if (= neg 0)
            (setf vj cidx
                  label 1) ;; (`center', `context'): positive sample
            (setf vj (neg-sampling-word w2v)
                  label 0))
        (when (or (= label 1) (/= vj cidx))
          (setf vj (%woutvi w2v vj))
          (setf yhat (cl-ml::sigmoid (vdot vj hiddens)))
          ;; eta * (yhat - label)
          (setf yhat (* eta (- yhat label)))
          ;; hgradsum := hgradsum + [eta * (yhat - label)] * vj
          (do-vector (i hidgrad)
            (incf (vref hidgrad i) (* yhat (vref vj i))))
          ;; vj := vj - [eta * (yhat - label)] * h
          (do-vector (i vj)
            (decf (vref vj i) (* yhat (vref hiddens i))))))
      (ecase algo
        (:cbow
         ;; update gradient for each context word
         (do ((context (- center randwin) (1+ context)))
             ((> context (+ center randwin)))
           (when (and (< -1 context (length sentence))
                      (/= context center))
             (setf hiddens (%winv w2v (nth context sentence)))
             (do-vector (i hiddens)
               ;; maybe we should multiply 1/cw on gradient
               (decf (vref hiddens i) (vref hidgrad i))))))
        (:skip-gram
         (do-vector (i hiddens)
           (decf (vref hiddens i) (vref hidgrad i))))))))

(defmethod fit-cbow ((w2v word2vec) sentence center randwin hiddens hidgrad)
  "CBOW using hierarchal softmax or negative sampling"
  (with-slots (hsoftmax negsample) w2v
    (let ((cw 0))
      (do-vector (i hiddens)
        (setf (vref hiddens i) 0))
      (do ((context (- center randwin) (1+ context)))
          ((> context (+ center randwin)))
        (when (and (< -1 context (length sentence))
                   (/= context center))
          (incf cw)
          (m+= hiddens (%winv w2v (nth context sentence)))))
      (when (> cw 0)
        (m/= hiddens cw)
        ;; NOTICE: `hidgrad' is gradient of `hiddens'
        ;;  for cbow, there is only one target word `center'
        (do-vector (i hidgrad)
          (setf (vref hidgrad i) 0))
        (cond
          (hsoftmax
           (hierarchal-softmax w2v sentence center randwin hiddens hidgrad))
          ((> negsample 0)
           (progn
             (negative-sampling w2v sentence center randwin hiddens hidgrad)))
          (t (error "one of :hsoftmax or :negsample needed for CBOW")))))))

(defmethod fit-skip-gram ((w2v word2vec) sentence center randwin hidgrad)
  "Skip-gram using hierarchal softmax or negative sampling
for :skip-gram, `hiddens' = (mrv in-matrix center)"
  (with-slots (hsoftmax negsample) w2v
    (let ((hiddens nil)
          ;; NOTICE: `hiddens' should have been vector of `center'
          ;;   in word2vec, we switch to `context', and `center' became the target word
          ;; (hiddens (%winv w2v (nth center sentence)))
          )
      (do ((context (- center randwin) (1+ context)))
          ((> context (+ center randwin)))
        (when (and (< -1 context (length sentence))
                   (/= context center))
          ;; NOTICE: `hidgrad' should have been gradient of `center' vector:
          ;;  summed across all context words
          ;; Now it is just gradient of `context':
          ;;  summed across huffman inner vector in hierarchal-softmax
          ;;  summed across `center' and negative sampling words in negative-sampling
          (setf hiddens (%winv w2v (nth context sentence)))
          (do-vector (i hidgrad)
            (setf (vref hidgrad i) 0))
          (cond
            (hsoftmax
             (hierarchal-softmax w2v sentence center randwin hiddens hidgrad))
            ((> negsample 0)
             (progn
               (negative-sampling w2v sentence center randwin hiddens hidgrad)))
            (t (error "one of :hsoftmax or :negsample needed for Skip-gram"))))))))

(defmethod %init-network ((w2v word2vec) vocab)
  (with-slots (dimensions vocabulary in-matrix out-matrix) w2v
    (setf vocabulary vocab)
    (with-slots (vocab-size) vocab
      (setf in-matrix (make-rand-matrix vocab-size dimensions))
      (setf out-matrix (make-rand-matrix dimensions vocab-size)))))

(defmethod %fit-from ((w2v word2vec) from type)
  (ecase type
    (:list (%init-network w2v (learn-vocab-from-list from)))
    (:file (%init-network w2v (learn-vocab-from-file from)))
    (:string (%init-network w2v (learn-vocab-from-string from))))
  (format t "Vocab size: ~A~%" (vocab-size (slot-value w2v 'vocabulary)))
  (with-slots (dimensions window max-sents algo negsample epochs) w2v
    (when (> negsample 0)
      (%init-negative-sampling w2v))
    (let (;; # of sentences, # of words, # of words in current sentence
          (nsents 0) (nwords 0) (ncwords 0)
          (corpus nil)
          ;; hidden layer vector and hidden layer gradient
          (hiddens (make-row-vector dimensions :initial-element 0))
          (hidgrad (make-vector dimensions :initial-element 0)))
      (dotimes (ep epochs)
        (ecase type
          (:file (setf corpus (open from)))
          (:string (setf corpus (make-string-input-stream from)))
          (:list (setf corpus from)))
        (do ((sentence (get-next-sentence corpus nsents :max-len max-sents)
                       (get-next-sentence corpus nsents :max-len max-sents)))
            ((eq sentence :eof))
          (incf nsents)
          (setf ncwords 0)
          (dotimes (center (length sentence))
            (incf nwords)
            (incf ncwords)
            (when (= (nth-value 1 (floor nwords 10000)) 0)
              ;; learn rate schedule every 1w words
              (learn-rate-schedule w2v nwords))
            (let* ((b (randint window))
                   ;; [-window, window] -> [-window + b, window - b]
                   (randwin (- window b)))
              (ecase algo
                (:cbow (fit-cbow w2v sentence center randwin hiddens hidgrad))
                ;; no need to fill `hiddens' for Skip-gram
                (:skip-gram (fit-skip-gram w2v sentence center randwin hidgrad))))))
        (case type
          ((:file :string) (close corpus)))))))

(defmethod fit ((w2v word2vec) X &optional y)
  "by default, fit from file"
  (declare (ignore y))
  (%fit-from w2v X :file))

(defmethod fit-from-string ((w2v word2vec) X &optional y)
  (declare (ignore y))
  (%fit-from w2v X :string))

(defmethod fit-from-list ((w2v word2vec) X &optional y)
  (declare (ignore y))
  (%fit-from w2v X :list))

(defmethod has-embedding ((w2v word2vec) X)
  (with-slots (vocabulary) w2v
    (has-word-in-vocab vocabulary X)))

(defmethod transform ((w2v word2vec) X)
  (with-slots (vocabulary in-matrix) w2v
    (let ((idx (get-word-indx vocabulary X)))
      (mrv in-matrix idx))))

(defmethod sim-word ((w2v word2vec) X &optional (n 10))
  (with-slots (vocabulary in-matrix) w2v
    (let ((idx (get-word-indx vocabulary X))
          (sims nil))
      (do-matrix-row (i in-matrix)
        (unless (= i idx)
          (push (cons i (cosine-distance (mrv in-matrix i) (mrv in-matrix idx))) sims)))
      (setf sims (sort sims #'> :key #'cdr))
      (mapcar (lambda (pair) (cons (get-indx-word vocabulary (car pair)) (cdr pair)))
              (subseq sims 0 n)))))
