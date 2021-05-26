;;;; Copyright (c) 2012-2015 jnjcc, Yste.org. All rights reserved.
;;;;

(asdf:defsystem #:cl-nlp
  :description "Natural Language Processing in Common Lisp"
  :version "0.1.0"
  :author "jnjcc at live.com"
  :licence "GPL"
  :depends-on (#:cl-ml)
  :serial t
  :components ((:module "algo"
                        :components ((:file "packages")
                                     (:file "distance")
                                     (:file "dict")
                                     (:file "bidict")
                                     ))
               (:file "packages")
               (:module "corpora"
                        :components ((:file "treebank")
                                     (:file "conllc")
                                     (:file "conllt")
                                     (:file "crfsuite")
                                     ))
               (:module "probs"
                        :components (
                                     ;; derived probability distribution from frequency distribution
                                     (:file "freqdist")
                                     (:file "binfreq")
                                     (:file "hashfreq")
                                     (:file "condfreq")
                                     (:file "derivdist")
                                     (:file "mutabledist")
                                     (:file "api")))
               (:module "pgm"
                        :components ((:file "hmm")
                                     (:file "crf")
                                     (:file "csgd")
                                     ))
               (:module "lm"
                        :components ((:file "wfreq")
                                     (:file "ngram")
                                     (:file "idvocab")
                                     (:file "idngram")
                                     (:file "sorted")
                                     (:file "reduce")
                                     (:file "foftab")
                                     (:file "idinfo")
                                     (:file "idtree")
                                     (:file "discount")
                                     (:file "katz")
                                     (:file "backoff")
                                     (:file "idlm")
                                     (:file "arpa")
                                     (:file "lm")
                                     ))
               (:module "segment"
                        :components ((:file "u8char")
                                     (:file "word")
                                     ))
               (:module "postag"
                        :components ((:file "tagger")
                                     (:file "hmmtag")
                                     (:file "crftag")
                                     ))
               (:module "ner"
                        :components ((:file "ner")
                                     ))
               (:module "embed"
                        :components ((:file "vocab")
                                     (:file "word2vec")
                                     (:file "node2vec")))))

(asdf:defsystem #:cl-nlp-test
  :description "cl-nlp test suite"
  :version "0.1.0"
  :author "jnjcc at live.com"
  :licence "GPL"
  :depends-on (#:cl-nlp #:lisp-unit)
  :serial t
  :components ((:module "test"
                        :components ((:file "packages")
                                     (:file "common")
                                     (:file "dataset")
                                     (:file "algo-test")
                                     (:file "lm-test")
                                     (:file "corpora-test")
                                     (:file "pgm-test")
                                     (:file "postag-test")
                                     (:file "word2vec-test")
                                     (:file "tests")))))
