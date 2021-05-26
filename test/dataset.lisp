;;;; Copyright (c) 2012-2015 jnjcc, Yste.org. All rights reserved.
;;;;

(in-package #:cl-nlp/test)

(defvar *dataset-path* (merge-pathnames "test/dataset/"
                                        (asdf:system-source-directory :cl-nlp-test)))

(defvar *nltk-path* (merge-pathnames "nltk_data/" (user-homedir-pathname)))

(defvar *treebank-dir* (merge-pathnames "corpora/treebank/combined/" *nltk-path*))
