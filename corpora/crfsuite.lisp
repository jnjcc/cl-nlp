;;;; Copyright (c) 2012-2015 jnjcc, Yste.org. All rights reserved.
;;;;
;;;; CRFsuite data format for CRF training and tagging
;;;;   (http://www.chokkan.org/software/crfsuite/manual.html)
;;;;
;;;; BNF notation representing the data format:
;;;;   <line>       ::= <item> | <eos>
;;;;   <item>       ::= <label> ('\t' <attribute>)+ <br>
;;;;   <eos>        ::= <br>
;;;;   <label>      ::= <string>
;;;;   <attribute>  ::= <name> | <name> ':' <scaling>
;;;;   <name>       ::= (<letter> | "\:" | "\\")+
;;;;   <scaling>    ::= <numeric>
;;;;   <br>         ::= '\n'

(in-package #:cl-nlp)

;;; CRFsuite jargons:
;;;   - `label': label for an `item', e.g. "B-NP"
;;;   - `token': `attr-name', or `attr-scale'
;;;   - `attribute': attribute for an `item', i.e. (attr-name . attr-scale)
;;;   - `item': label (possibly with word itself) and attributes, e.g. (("B-NP" "Confidence") attr attr ...)
;;;   - `instance': a list of `item', (item item ...)
;;;   - `dataset': a list of `instance', (instance instance ...)
(defun read-crf-label (stream)
  (let ((chars nil))
    (labels ((special (x)
               (or (char= x #\Tab) (char= x #\Newline))))
      (do-char-until (chr stream #'special)
        (push chr chars)))
    (when chars
      (join-strings (nreverse chars) ""))))

(defun read-crf-token (stream)
  "reads `attr-name', or `attr-scale'"
  (let ((chars nil))
    (labels ((special (x)
               ;; colon has special meanings
               (or (char= x #\:) (char= x #\Tab) (char= x #\Newline))))
      (do-char-until (chr stream #'special)
        (when (char= chr #\\)
          (let ((next (peek-next-char stream nil :eof)))
            ;; escaping #\\ and #\:
            (when (or (eq next #\\) (eq next #\:))
              (setf chr (read-char stream)))))
        (push chr chars)))
    (when chars
      (join-strings (nreverse chars) ""))))

(defun read-crf-attribute (stream)
  "reads (`attr-name' . `attr-scale') pair:
if `attr-name' nil, we are reaching eof or end-of-item"
  (discard-chars-if stream (lambda (x) (char= x #\Tab)))

  (let ((next (peek-next-char stream nil :eof)))
    (when (or (eq next :eof) (eq next #\Newline))
      (discard-next-char stream nil)
      (return-from read-crf-attribute (values nil nil))))

  (let ((attr-name (read-crf-token stream))
        (attr-scale 1.0)
        (next (peek-next-char stream nil :eof)))
    (when (eq next #\:)
      (setf attr-scale (read-from-string (read-crf-token stream))))
    (values attr-name attr-scale)))

(defun read-crf-item (stream &optional crfdata)
  "Returns: :eof if end-of-file; :eol if end-of-line; item otherwise;
where `crfdata' being object of `crfsuite-dataset', or nil"
  (let ((next (peek-next-char stream nil :eof)))
    (cond
      ((eq next :eof) (return-from read-crf-item :eof))
      ((eq next #\Newline) (progn
                             (discard-next-char stream)
                             (return-from read-crf-item :eol)))))
  ;; item = (("B-NP" ["Confidence"]) attr attr ...)
  (let ((item nil)
        (labl (read-crf-label stream)))
    (when crfdata
      (setf labl (crfdata-aget-lid crfdata labl)))
    (setf item (list (list labl)))
    ;; forever-loop
    (do () (nil)
      (multiple-value-bind (name scale) (read-crf-attribute stream)
        (when (null name)
          (return))
        (when crfdata
          (setf name (crfdata-aget-aid crfdata name)))
        (push (cons name scale) item)))
    (nreverse item)))

(defun read-crf-instance (stream &optional crfdata)
  "Returns :eof if no more instance; or list of items"
  ;; skip empty lines until the next instance
  (discard-chars-if stream (lambda (x) (char= x #\Newline)))
  (let ((inst nil)
        (eof-item nil))
    (do ((item (read-crf-item stream crfdata) (read-crf-item stream crfdata)))
        ((eq item :eol))
      (when (eq item :eof)
        (setf eof-item t)
        (return))
      (push item inst))
    (if (and eof-item (null inst))
        :eof
        (nreverse inst))))

(defun read-crf-datalist (stream)
  "Returns: dataset as list"
  (let ((dataset nil))
    (do ((inst (read-crf-instance stream) (read-crf-instance stream)))
        ((eq inst :eof))
      (push inst dataset))
    (nreverse dataset)))

(defclass crfsuite-dataset ()
  ((instances :initform nil :reader instances
              :documentation "list of training instances, with string replaced by index")
   (attr-bhdict :initform (make-bhdict) :documentation "attr-name <-> index")
   (labl-bhdict :initform (make-bhdict) :documentation "labl-name <-> idnex")))

(defmethod crfdata-aget-aid ((crfdata crfsuite-dataset) aname)
  "Returns: attr-idx `aid' for `aname'
NOTICE: will add attr-name `aname' if not exist"
  (with-slots (attr-bhdict) crfdata
    (let ((aid (bhdict-ref attr-bhdict aname)))
      (unless aid
        (setf aid (bhdict-len attr-bhdict))
        (bhdict-add attr-bhdict aname aid))
      aid)))

(defmethod crfdata-get-aid ((crfdata crfsuite-dataset) aname)
  (with-slots (attr-bhdict) crfdata
    (bhdict-ref attr-bhdict aname)))

(defmethod crfdata-get-aname ((crfdata crfsuite-dataset) aid)
  (with-slots (attr-bhdict) crfdata
    (bhdict-bref attr-bhdict aid)))

(defmethod crfdata-aget-lid ((crfdata crfsuite-dataset) labl)
  "Returns: label-idx `lid' for `labl'
NOTICE: will add `labl' if not exists"
  (with-slots (labl-bhdict) crfdata
    (let ((lid (bhdict-ref labl-bhdict labl)))
      (unless lid
        (setf lid (bhdict-len labl-bhdict))
        (bhdict-add labl-bhdict labl lid))
      lid)))

(defmethod crfdata-get-lid ((crfdata crfsuite-dataset) labl)
  (with-slots (labl-bhdict) crfdata
    (bhdict-ref labl-bhdict labl)))

(defmethod crfdata-get-labl ((crfdata crfsuite-dataset) lid)
  (with-slots (labl-bhdict) crfdata
    (bhdict-bref labl-bhdict lid)))

(defmethod crfdata-set-insts ((crfdata crfsuite-dataset) instlst)
  (with-slots (instances) crfdata
    (setf instances instlst)))

(defun read-crf-dataset (stream)
  "Returns: dataset as object"
  (let ((crfdata (make-instance 'crfsuite-dataset))
        (instlst nil))
    (do ((inst (read-crf-instance stream crfdata) (read-crf-instance stream crfdata)))
        ((eq inst :eof))
      (push inst instlst))
    (crfdata-set-insts crfdata instlst)
    crfdata))

(defmacro do-crf-instance ((inst crfdata) &body body)
  `(dolist (,inst (instances ,crfdata))
     ,@body))

(defmacro do-crf-item ((lid attrs inst) &body body)
  (let ((item (gensym "ITEM-")))
    `(let ((,lid nil)
           (,attrs nil))
       (dolist (,item ,inst)
         (setf ,lid (caar ,item))
         (setf ,attrs (cdr ,item))
         ,@body))))

(defmacro do-crf-attr ((aid scale attrs) &body body)
  (let ((attr (gensym "ATTR-")))
    `(let ((,aid nil)
           (,scale nil))
       (dolist (,attr ,attrs)
         (setf ,aid (car ,attr))
         (setf ,scale (cdr ,attr))
         ,@body))))
