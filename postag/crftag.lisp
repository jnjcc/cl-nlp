;;;; Copyright (c) 2012-2015 jnjcc, Yste.org. All rights reserved.
;;;;
;;;; Conditional Random Fields Tagger

(in-package #:cl-nlp)

(defclass crf-tagger (pos-tagger crf-model)
  ())

(defmethod crf-tag-sent ((crf crf-tagger) sent)
  )
