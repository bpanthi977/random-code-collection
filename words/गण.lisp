(ql:quickload '(:serapeum :alexandria))

(uiop:define-package #:छन्द/utils
    (:use :cl :serapeum :alexandria)
  (:reexport :serapeum :alexandria))

(defpackage #:छन्द
  (:use :cl)
  (:local-nicknames (#:u #:छन्द/utils)))

(in-package #:छन्द)

(defun replace-all (pattern result seq)
  "Replace all occurance of `pattern' with `result' in sequence `seq'"
  (u:if-let (pos (search pattern seq))
    (values (nconc (u:firstn pos seq)
		   (list result)
		   (replace-all pattern result (nthcdr (+ pos (length pattern)) seq)))
	    t)
    (values seq nil)))

(defparameter *file* #p"~/lisp/archive/dict/dict/dict-dump/modified-dict")
(defparameter *dict* (uiop:read-file-form *file*))

(defparameter *laghu* (list #\ि #\ु #\ँ #\ृ))
(defparameter *laghu-letter* (list #\अ #\इ #\उ #\ऋ))
(defparameter *guru* (list #\ा #\ी #\ू #\े #\ै #\ो #\ौ #\ं #\: #\DEVANAGARI_SIGN_VISARGA))
(defparameter *guru-letter* (list #\आ #\ई #\ऊ #\ए #\ऐ #\ओ #\औ))
(defparameter *viram* #\DEVANAGARI_SIGN_VIRAMA)

(defun canonicalize (char)
  (cond ((eql char *viram*) :्)
	((find char *guru*) :ी)
	((find char *guru-letter*) :ई)
	((find char *laghu*) :ि)
	(t :क)))

(defparameter *rules* '((:क् :क :्)
			(:क् :क् :क्)
			(:ई :क :क्)
			(:ई :ई :क्)
			(:क :क :ि)
			(:ई :क :ी)))

(defun transform (seq)
  (loop for (result . pattern) in *rules*
	with changed = nil do  
	  (multiple-value-bind (new replaced?) (replace-all pattern result seq)
	    (when replaced? (setf changed t))
	    (setf seq new))
	finally (return (if changed
			    (transform seq)
			    seq))))

(defun मात्रा (word)
  (transform (map 'list #'canonicalize word)))

(defun cleanup (word)
  (let ((dirty #.(concatenate 'string "१२३४५६७८९०;-।.~[]()=?,/"
			      (list #\Tab #\Newline #\Return)
			      "abcdefghijklmnopqrstuvwxyz"
			      "ABCDEFGHIJKLMNOPQRSTUVWXYZ"
			      "±√!@#$%^&_=¬²'´［］½×Å˜×Þ")))
    (remove-if (lambda (c)
		 (find c dirty :test #'char=))
	       word)))

(defun find-गण (गण)
  (loop for (word . meaning) in *dict*
	when (equal (मात्रा (cleanup word)) गण)
	  collect word))

(defparameter *words* (make-hash-table :test #'equal))

(defstruct word
  (edges (list) :type list)
  (गण nil :type list)
  (word "" :type string)
  (meaning "" :type string)
  (popularity 1 :type integer))

(defmethod print-object ((w word) stream)
  (print-unreadable-object (w stream)
    (format stream "~a" (word-word w))))

(defun calculate-popularities ()
  ;; reset
  (maphash (lambda (key word)
	     (declare (ignore key))
	     (setf (word-popularity word) 1))
	   *words*)
  ;; compute
  (maphash (lambda (key word)
	     (declare (ignore key))
	     (loop for e in (word-edges word) do
	       (incf (word-popularity e))))
	   *words*))

(defun get-word (word &optional (table *words*))
  (or (gethash word table)
      (let ((w (make-word :गण (मात्रा word)
			  :edges nil
			  :word word)))
	(setf (gethash word table) w)
	w)))

(defun update-edges (word new-edges table)
    (with-slots (edges) word 
      (loop for e in new-edges do 
	(pushnew (get-word e table) edges :test #'equal))))

  (defun create-graph ()
    (loop with table = (make-hash-table :test #'equal :size (expt 10 6))
	  for (w meaning . rest) in *dict*
	  for relatives = (remove-if #'null (mapcar #'cleanup (uiop:split-string meaning :separator " ।.-;")))
	  do
	     (let ((words (uiop:split-string w :separator "/")))
	       ;; some dictinary entries are like अँगरखा/अँगर्खा
	       (loop for word in words
		     for w = (get-word (cleanup word) table) do
		       (update-edges w relatives table)
		       (setf (word-meaning w) meaning)))
	  finally (setf *words* table)))

(defun गणp-f (गण)
  "returns a function that check if its argument is of given `गण'"
  (lambda (word)
    (equal (word-गण word) गण)))

(defun filter (word depth filter-function &optional bag)
  "Collect words in the word graph that satisfy `filter' by travesing edges upto `depth'"
  (cond ((= depth 0)
	 bag)
	(t
	 (loop for w in (word-edges word) do
	   (when (funcall filter-function w)
	     (pushnew w bag))
	   (setf bag (filter w (1- depth) filter-function bag)))
	 bag)))

(defun κ (word1 word2)
  (if (eql word1 word2)
      :infinity 
      (max (if (find word2 (word-edges word1)) (/ 1 (word-popularity word2)) 0)
	   (if (find word1 (word-edges word2)) (/ 1 (word-popularity word1)) 0))))

(defun κ* (k1 k2)
  (cond ((eql :infinity k1) k2)
	((eql :infinity k2) k1)
	(t (* k1 k2))))

(defun κ+ (k1 k2)
  (min (+ k1 k2) 1))

(defun filter2% (word depth filter-function &optional bag closeness-table)
  (declare (optimize (speed 0) (debug 3) (safety 3)))
  (labels ((c (w)
	     (gethash w closeness-table 0))
	   (κ++ (w increment)
	     (setf (gethash w closeness-table)
		   (κ+ (c w)
		       increment))))
    (cond ((= depth 0)
	   bag)
	  (t
	   (loop for w in (word-edges word) do
	     (when (funcall filter-function w)
	       (pushnew w bag))
	     (unless (or (eql w word) (eql (c w) :infinity))
	       (κ++ w (κ* (κ w word) (c word)))
	       (setf bag (filter2% w (1- depth) filter-function bag closeness-table))))
	   bag))))

(defun filter2 (word depth filter-function)
  (let ((closeness-table (make-hash-table)))
    (setf (gethash word closeness-table) :infinity)
    (sort (mapcar (lambda (w) 
		    (cons w (float (gethash w closeness-table))))
		  ;; filter then remove yourself if present
		  (remove word (filter2% word depth filter-function () closeness-table)))
	  ;; sort by closeness-table value
	  #'> :key #'cdr)))
