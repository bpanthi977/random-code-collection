(defpackage :aoc3
  (:use :cl :aoc))

(in-package :aoc3)

(defun make-grid (input)
  (make-array (list (length input) (length (first input)))
	      :initial-contents input))

(defparameter *input* (make-grid (input 3 :lines)))

(defun tree? (i j)
  (char= #\# (aref *input* i (mod j (array-dimension *input* 1)))))

(defun solve1 (&optional (istep 1) (jstep 3)) 
  (loop for i from 0 below (array-dimension *input* 0) by istep
	for j from 0 by jstep 
	count (tree? i j)))

(defun solve2 () 
  (* (solve1 1 1)
     (solve1 1 3)
     (solve1 1 5)
     (solve1 1 7)
     (solve1 2 1)))
