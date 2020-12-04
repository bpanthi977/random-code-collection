(defpackage :aoc1 
  (:use :cl :aoc))

(in-package :aoc1)

(defparameter *input* (input 1))

(defun solve1 ()
  (loop for (a . tail) on *input* 
	for b = (- 2020 a)
	when (find b tail)
	  return (* a b)))

(defun solve2 () 
  (loop for sublist on *input* 
	for a = (first sublist) do
    (loop for b in sublist 
	  for c = (- 2020 a b)
	  when (find c sublist)
	    do (return-from solve2 (* a b c)))))
