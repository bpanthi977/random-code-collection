(defpackage :aoc8
  (:use :cl :aoc))

(in-package :aoc8)

(defparameter *input* (input 8 :lines))

(defun parse-input (input) 
  (let ((instructions (make-array (length input))))
    (loop for inst in input 
	  for i from 0 
	  for n = (parse-integer inst :start 3) do 
	    (setf (aref instructions i) 
		  (cond ((search "nop" inst) (list :nop n))
			((search "acc" inst) (list :acc n))
			((search "jmp" inst) (list :jmp n))
			(t (error "Invalid instruction")))))
    instructions))

(defun run (program) 
  (loop with i = 0 
	with acc = 0 
	for (op value) = (aref program i) 
	unless op 
	  return (values acc nil)
	do 
	   (setf (aref program i) nil)
	   (case op 
	     (:nop nil)
	     (:acc (incf acc value))
	     (:jmp (incf i (1- value))))
	   (incf i)
	   (if (= i (1- (length program)))
	       (return (values acc t)))))

(defun solve1 ()
  (run (parse-input *input*)))

(defun solve2 () 
  "Bruteforce method"
  (let ((program (parse-input *input*)))
    (loop for i from 0 below (length program) 
	  for (op value) = (aref program i) do 
	    (when (member op (list :nop :jmp))
	      (let ((copy (copy-seq program)))
		(setf (aref copy i)
		      (if (eql op :nop) 
			  (list :jmp value)
			  (list :nop)))
		(multiple-value-bind (value natural-termination) (run copy)
		  (when natural-termination 
		    (return value))))))))
