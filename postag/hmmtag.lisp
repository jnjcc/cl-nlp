;;;; Copyright (c) 2012-2015 jnjcc, Yste.org. All rights reserved.
;;;;
;;;; Hidden Markov Model Tagger

(in-package #:cl-nlp)

(defclass hmm-tagger (pos-tagger hmm-model)
  ())

(defmethod pos-tag-sent ((hmm hmm-tagger) sent)
  (hmm-decode hmm sent))
