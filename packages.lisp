;;;; Copyright (c) 2012-2015 jnjcc, Yste.org. All rights reserved.
;;;;
;;;; Natural Language Processing in Common Lisp

(in-package #:cl-user)

(defpackage #:cl-nlp
  (:nicknames #:nlp)
  (:use #:cl
        #:cl-ml/probs #:cl-ml/algo #:cl-ml/linalg #:cl-ml/graph #:cl-ml/io #:cl-ml
        #:cl-nlp/algo)
  (:export #:fit #:transform

           ;;; Probabilistic Graphical Models
           #:hmm-model #:hmm-states #:hmm-symbols
           #:hmm-start-prob #:hmm-trans-prob #:hmm-emiss-prob
           #:hmm-forward #:hmm-backward #:hmm-decode #:hmm-learn

           ;;; Language Modeling
           ;; word frequency, from text stream
           #:word-frequency
           ;; word id vocab, from word frequency, or from file stream
           #:wfreq-idvocab #:read-idvocab
           ;; ngram (wngram or idngram), from text stream (and idvocab)
           #:wngram-count #:idngram-count #:wngram-idngram
           ;; reduce n-gram to m-gram
           #:ngram-mgram #:sorted-wngram #:sorted-idngram
           ;; frequency of frequency for 1-, 2-, ..., n-gram
           #:fof-from-ngram #:fof-ref
           #:write-ngram #:read-ngram
           #:idngram-lm #:ngram-lm

           ;;; Segment words
           #:segment-chars

           ;;; Part-of-Speech tagging
           #:pos-tagger #:hmm-tagger
           #:pos-tag-sent #:pos-tag-corpus #:pos-evaluate

           ;;; Embedding
           #:word2vec
           #:fit-from-string #:fit-from-list
           #:has-embedding
           #:node2vec
           ))
