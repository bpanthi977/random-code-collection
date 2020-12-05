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
  (loop with list = (mapcar #'id *input*)
	for i from 0 below (expt 2 10)
	with lastfound = nil do 
	(if (find i list)
	    (setf lastfound t)
	    (when lastfound 
	      (return i)))))


    
