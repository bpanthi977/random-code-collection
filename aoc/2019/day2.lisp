(defpackage :aoc2
  (:use :cl :aoc))

(in-package :aoc2)

(defun create-memory (input)
  (let* ((ints (uiop:split-string input :separator ","))
	 (mem (make-array (length ints) 
			  :element-type 'integer)))
    (loop for i from 0 
	  for int in (uiop:split-string input :separator ",") do 
	    (setf (aref mem i) (parse-integer int)))
    mem))

(defparameter +input+ (input 2 :string))

(defun run (mem)
  (macrolet ((arg1 () 
	       `(aref mem (aref mem (+ pointer 1))))
	     (arg2 () 
	       `(aref mem (aref mem (+ pointer 2))))
	     (result-loc () 
	       `(aref mem (+ pointer 3))))
    (do ((pointer 0 (+ pointer 4))
	 opcode)
	(nil)
      (setf opcode (aref mem pointer))
      (cond
	((eql opcode 1)
	 (setf (aref mem (result-loc)) (+ (arg1) (arg2))))
	((eql opcode 2)
	 (setf (aref mem (result-loc)) (* (arg1) (arg2))))
	((eql opcode 99)
	 (return mem))
	(t 
	 (error "invalid opcode ~d" opcode))))))


(defun solve1 () 
  (let ((mem (create-memory +input+)))
    (setf (aref mem 1) 12)
    (setf (aref mem 2) 2)
    (run mem)
    (aref mem 0)))

(defun solve2 ()
  (let ((memory (create-memory +input+)))
    (loop for noun from 0 to 99 do 
	  (loop for verb from 0 to 99 
		for mem = (copy-seq memory) do 
		(setf (aref mem 1) noun)
		(setf (aref mem 2) verb)
		(run mem)
		(when (= (aref mem 0) 19690720)
		  (return-from solve2 (+ (* 100 noun) verb)))))))
