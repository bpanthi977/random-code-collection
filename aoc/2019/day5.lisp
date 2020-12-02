(defpackage :aoc5
  (:use :cl :aoc))

(in-package :aoc5)

(defun create-memory (input)
  (let* ((ints (uiop:split-string input :separator ","))
	 (mem (make-array (length ints) 
			  :element-type 'integer)))
    (loop for i from 0 
	  for int in (uiop:split-string input :separator ",") do 
	    (setf (aref mem i) (parse-integer int)))
    mem))

(defparameter +input+ (input 5 :string))

(defun get-input () 
  (format t "~&Input: ")
  (read))

(defun output (n)
  (print n))

(defun run (mem)
  (macrolet ((param-mode (n) 
	       "n=1,2,3"
	       `(truncate (mod (aref mem pointer) ,(expt 10 (+ 2 n))) ,(expt 10 (+ 1 n))))
	     (arg1 () 
	       `(if (= (param-mode 1) 0)
		    (aref mem (aref mem (+ pointer 1)))
		    (aref mem (+ pointer 1))))
	     (arg2 () 
	       `(if (= (param-mode 2) 0)
		    (aref mem (aref mem (+ pointer 2)))
		    (aref mem (+ pointer 2))))
	     (result-loc () 
	       `(aref mem (+ pointer 3))))
    (do ((pointer 0 (+ pointer (case opcode 
				 ((1 2 7 8) 4)
				 ((3 4) 2)
				 ((5 6) 0)
				 (t 0))))
	 opcode)
	(nil)
      (setf opcode (mod (aref mem pointer) 100))
      (cond
	((eql opcode 1)
	 (setf (aref mem (result-loc)) (+ (arg1) (arg2))))
	((eql opcode 2)
	 (setf (aref mem (result-loc)) (* (arg1) (arg2))))
	((eql opcode 3)
	 (setf (aref mem (aref mem (+ pointer 1))) (get-input)))
	((eql opcode 4)
	 (output (arg1)))
	((eql opcode 5)
	 (if (not (= 0 (arg1)))
	     (setf pointer (arg2))
	     (incf pointer 3)))
	((eql opcode 6)
	 (if (= 0 (arg1))
	     (setf pointer (arg2))
	     (incf pointer 3)))
	((eql opcode 7)
	 (if (< (arg1) (arg2))
	     (setf (aref mem (result-loc)) 1)
	     (setf (aref mem (result-loc)) 0)))
	((eql opcode 8)
	 (if (= (arg1) (arg2))
	     (setf (aref mem (result-loc)) 1)
	     (setf (aref mem (result-loc)) 0)))
	((eql opcode 99)
	 (return mem))
	(t 
	 (error "invalid opcode ~d" opcode))))))


(defun solve1 (&optional (input +input+))
  (let ((mem (create-memory input)))
    (run mem)
    nil))


