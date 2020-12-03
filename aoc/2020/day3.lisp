(defpackage :aoc3
  (:use :cl :aoc))

(in-package :aoc3)

(defun make-grid (input)
  (let ((grid (make-array (list (length input) (length (first input)))
			  :element-type 'boolean :initial-element nil)))
    (loop for row in input 
	  for i from 0 do 
	  (loop for char across row 
		for j from 0 do 
		(setf (aref grid i j) 
		      (if (eql char #\.)
			  nil 
			  t))))

    grid))

(defparameter *input* (make-grid (input 3 :lines)))

(defun tree? (i j)
  (aref *input* i (mod j (array-dimension *input* 1))))

(defun solve1 (&optional (istep 1) (jstep 3)) 
  (loop for i from 0 below (array-dimension *input* 0) by istep
	for j from 0 by jstep 
	with count = 0 do 
	  (when (tree? i j)
	    (incf count))
	finally (return count)))

(defun solve2 () 
  (* (solve1 1 1)
     (solve1 1 3)
     (solve1 1 5)
     (solve1 1 7)
     (solve1 2 1)))
