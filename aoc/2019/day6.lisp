(defpackage :aoc6
  (:use :cl :aoc))

(in-package :aoc6)

(defparameter +input+ (input 6 :lines))

(defun parse-orbits (lines)
  (let ((table (make-hash-table :test #'equal :size (length lines))))
    (loop for line in lines 
	  for orbit = (position #\) line)
	  for orbitee = (subseq line 0 orbit)
	  for orbiter = (subseq line (1+ orbit)) do 
	    (setf (gethash orbiter table) orbitee))
    table))

(defparameter *orbit* (parse-orbits +input+))
(defparameter *orbit-level* (make-hash-table :test #'equal :size (length +input+)))

(defun orbitee (orbiter)
  (gethash orbiter *orbit*))

(defun orbit-level (orbiter) 
  (if (string= orbiter "COM")
      0
      (or (gethash orbiter *orbit-level*)
	  (let ((orbitee (gethash orbiter *orbit*)))
	    (setf (gethash orbiter *orbit-level*) (+ 1 (orbit-level orbitee)))))))

(defun solve1 ()
  (let ((count 0))
    (maphash (lambda (orbiter orbitee)
	       (declare (ignore orbitee))
	       (incf count (orbit-level orbiter)))
	     *orbit*)
    count))
      
    
(defun jumps (a b) 
  "number of orbital jumps between `a' and `b' "
  (let ((ola (orbit-level a))
	(olb (orbit-level b)))
    (cond ((string= a b) 0)
	  ((> ola olb)
	   (+ 1 (jumps (orbitee a) b)))
	  ((> olb ola)
	   (+ 1 (jumps a (orbitee b))))
	  ((= olb ola)
	   (+ 2 (jumps (orbitee a) (orbitee b)))))))

(defun solve2 ()
  (jumps (orbitee "SAN") (orbitee "YOU")))
