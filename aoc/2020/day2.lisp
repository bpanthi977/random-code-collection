(defpackage :aoc2
  (:use :cl :aoc))

(in-package :aoc2)

(defparameter *input* (input 2 :lines))

(defun count-valid (validp)
  (let ((valid 0))
    (ppcre:do-register-groups ((#'parse-integer lo hi) ch pwd)
        ("(?m)^(\\d+)-(\\d+) (\\w): (\\w+)$"
         (uiop:read-file-string #P"day02.txt")
         valid)
      (when (funcall validp lo hi (char ch 0) pwd)
         (incf valid)))))

(defun map-row (function lines)
  (let ((scanner (ppcre:create-scanner "(\\d+)-(\\d+) (\\w): (\\w*)")))
    (loop for input in lines do 
      (ppcre:register-groups-bind (lower upper charstring password)
	  (scanner input :sharedp t)

	(if (and lower upper charstring password)
	    (funcall function 
		     (parse-integer lower)
		     (parse-integer upper) 
		     (char charstring 0) 
		     password)
	    (error "invalid input"))))))	

(defun solve1 ()
  (let ((count 0))
    (map-row (lambda (lower upper char password)
	       (when (<= lower (count char password) upper)
		 (incf count)))
	     *input*)
    count))

(defun xor (a b)
  (and (or a b) 
       (or (not a)
	   (not b))))

(defun solve2 () 
  (let ((count 0))
    (map-row (lambda (lower upper char password)
	       (when (xor (char= (char password (1- lower)) char)
		   (char= (char password (1- upper)) char))
		 (incf count)))
	     *input*)
    count))
    
