(defpackage :aoc6 
  (:use :aoc :cl))

(in-package :aoc6)

(defparameter *input* (input 6 :string))

(defun parse-group-input (input)
  (loop with answers = (make-array 26 :element-type 'bit :initial-element 0)
	for char across input do 
	  (unless (char= char #\Newline)
	    (setf (aref answers (- (char-code char) #.(char-code #\a))) 1))
	finally (return answers)))

(defun solve1 () 
  (let ((groups (ppcre:split "\\n\\n" *input* :sharedp t)))
    (loop for group in groups 
	  summing (count 1 (parse-group-input group)))))
	
(defun parse-group-input2 (input)
  (loop with answers = (make-array 26 :element-type 'bit :initial-element 0)
	with lines = (ppcre:split "\\n" input :sharedp t)
	for char across (first lines) do 
	  (when (every #'(lambda (l) (find char l)) 
		       (rest lines))
	    (setf (aref answers (- (char-code char) #.(char-code #\a))) 1))
	finally (return answers)))

(defun solve2 () 
  (let ((groups (ppcre:split "\\n\\n" *input* :sharedp t)))
    (loop for group in groups 
	  summing (count 1 (parse-group-input2 group)))))
