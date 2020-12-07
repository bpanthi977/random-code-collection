(defpackage :aoc7 
  (:use :cl :aoc))

(in-package :aoc7)

(defparameter *input* (input 7 :lines))

;;; The rules given describe a graph 
;;; with bag colors as vertices and 
;;; a number associated with each edge

(defparameter *containers* (make-hash-table :test #'equal))
(defparameter *contents* (make-hash-table :test #'equal))

(defun containers (content)
  (gethash content *containers*))

(defun contents (container)
  (gethash container *contents*))

(defun collect-bags (input &key (start 0))
  (let ((pos (search "bag" input :start2 start)))
    (when pos 
      (let ((digit-pos (position-if #'digit-char-p input :start start :end pos)))
	(multiple-value-bind (n p) (if digit-pos 
				       (parse-integer input :start digit-pos :junk-allowed t)
				       (values nil start))
	      (cons (cons n (subseq input
				    (position-if #'alpha-char-p input :start p)
				    (1+ (position-if #'alpha-char-p input :end pos :from-end t))))
		    (collect-bags input :start (1+ pos))))))))
	
(defun make-graph (input) 
  (loop for i in input
	for bags = (collect-bags i)
	for container = (cdr (first bags)) do
	  (loop for  n.content in (rest bags) do
	    (when (car n.content)
	      (pushnew container (gethash (cdr n.content) *containers*) :test #'string=)
	      (pushnew n.content (gethash container *contents*) :test #'equal)))))

(defun solve1% (bag bags)
  (cond ((find bag bags :test #'string=)
	 bags)
	(t 
	 (setf bags (cons bag bags))
	 (loop for b in (containers bag) do 
	   (setf bags (solve1% b bags)))
	 bags)))

(defun solve1 ()
  (1- (length (solve1% "shiny gold" nil))))

(defun solve2 (bag) 
  (loop for (n . b) in (contents bag) 
	summing (+ n (* n (solve2 b)))))


