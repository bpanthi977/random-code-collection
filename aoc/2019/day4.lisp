(defpackage :aoc4
  (:use :cl :aoc))

(in-package :aoc4)

(defparameter +input+ (list 231832 767346))

(defun split-number (n)
  (loop for l = (mod n 10)
	do (setf n (truncate n 10))
	collect l into list 
	when (= n 0)
	  return (nreverse list)))


(defun valid? (n)
  (let ((split (split-number n)))
    (loop for d in split 
	  with rep = nil
	  for dd in (rest split) do 
	    (unless (<= d dd)
	      (return nil))
	    (if (= d dd)
		(setf rep t))
	  finally (return rep))))

(defun valid2? (n)
  (let ((split (split-number n)))
    (loop for d in split 
	  for i from 2
	  with rep = nil
	  with dp = nil ;; digit previous
	  for dd in (rest split) do 
	    (unless (<= d dd)
	      (return nil))
	    (if (and (= d dd)
		     (or (not dp) (not (= dp d)))
		     (or (>= i (length split)) (not (= d (nth i split)))))
		(setf rep t))
	    (setf dp d)
	  finally (return rep))))

(defun solve1 ()
  (loop for x from (first +input+) to (second +input+) 
	with count = 0 do 
	  (when (valid? x)
	    (incf count))
	finally (return count)))

(defun solve2 ()
  (loop for x from (first +input+) to (second +input+)
	with count = 0 do 
	(when (valid2? x)
	  (incf count))
	finally (return count)))


	
