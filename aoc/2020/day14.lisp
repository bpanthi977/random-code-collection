(defpackage :aoc14
  (:use :cl :aoc)
  (:local-nicknames (:a :alexandria)))

(in-package :aoc14)

(defparameter *input* (input 14 :lines))

(defstruct mask
  one
  zero)

(defun parse-line (line)
  (if (a:starts-with-subseq "mask" line)
      (loop for i from 7 below (length line)
	    for char = (char line i)
	    for k from 35 downto 0
	    with one-mask = 0
	    with zero-mask = (1- (expt 2 36)) do 
	      (case char 
		    (#\X nil)
		    (#\1 (setf (ldb (byte 1 k) one-mask) 1))
		    (#\0 (setf (ldb (byte 1 k) zero-mask) 0))
		    (t (print char)))
	      finally (return (make-mask :one one-mask :zero zero-mask)))
      (multiple-value-bind (addr pos) (parse-integer line :start 4 :junk-allowed t)
	(list addr
	      (parse-integer line :start (+ pos 4))))))

(defun apply-mask (mask val)
  (logior (logand val (mask-zero mask))
	  (mask-one mask)))

(defun solve1 ()
  (let ((memory (make-hash-table)))
    (loop for line in *input*
	  for input = (parse-line line) 
	  with mask = nil do
	    (if (mask-p input)
		(setf mask input)
		(destructuring-bind (addr val) input
		    (setf (gethash addr memory) (apply-mask mask val)))))
    (loop for val being the hash-value of memory 
	  summing val)))

(defun floating-bits (mask)
  (logand (mask-zero mask)
	  (lognot (mask-one mask))))

(defun modify-memory% (memory fbs addr val)
  (if fbs
      (let ((pos (first fbs)))
	(setf (ldb (byte 1 pos) addr) 0)
	(modify-memory% memory (rest fbs) addr val)
	(setf (ldb (byte 1 pos) addr) 1)
	(modify-memory% memory (rest fbs) addr val))
      (setf (gethash addr memory) val)))

(defun modify-memory (memory fbs addr val)
  (let ((fbs (loop for k from 35 downto 0
		       when (logbitp k fbs)
			 collect k)))
    (modify-memory% memory fbs addr val)))


(defun solve2 () 
  (let ((memory (make-hash-table)))
    (loop for line in *input*
	  for input = (parse-line line) 
	  with mask = nil do
	    (if (mask-p input)
		(setf mask input)
		(destructuring-bind (addr val) input
		  (modify-memory memory
				 (floating-bits mask)
				 (logior (mask-one mask) addr)
				 val))))
    (values (loop for val being the hash-value of memory 
		  summing val)
	    memory)))




