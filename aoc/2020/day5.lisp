(defpackage :aoc5
  (:use :cl :aoc))

(in-package :aoc5)

(defparameter *input* (input 5 :lines))

(defun id (input) 
  (loop with id = 0
	for i from 9 downto 0
	for char across input do 
	  (if (or (char= char #\B) (char= char #\R))
	      (incf id (expt 2 i)))
	finally (return id)))

(defun solve1 ()
  (reduce #'max *input* :key #'id))

(defun solve2 ()
  (let ((seats (make-array (expt 2 10) :element-type 'bit :initial-element 0)))
    (map 'nil (lambda (input) 
		(setf (aref seats (id input)) 1)) 
	 *input*)
    (1+ (search #*101 seats))))


    
