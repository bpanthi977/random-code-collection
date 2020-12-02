(defpackage :aoc3
  (:use :cl :aoc))

(in-package :aoc3)

(defparameter +input+ (input 3 :lines))

(defun path-to-vertices (input)
  (loop with pos = 0 
	with length = (length input)
	with x = 0 
	with y = 0 
	for char = (char input pos) do 
	  (multiple-value-bind (steps newpos) 
	      (parse-integer input :start (1+ pos) :junk-allowed t)
	    (setf pos (1+ newpos))
	    
	    (case char 
	      (#\R (incf x steps))
	      (#\L (decf x steps))
	      (#\U (incf y steps))
	      (#\D (decf y steps))))
	collect (cons x y) into vertices
	when (> pos length)
	  return vertices))

(defun in-order (a b c)
  (or (<= a b c)
      (<= c b a)))

(defun absmin% (a b)
  (if (< (abs a) (abs b))
      a b))

(defun absmin (&rest values)
  (reduce #'absmin% values))

(defun inters (e11 e12 e21 e22)
  "Find intersection point between edge (e11 e12) and (e21 e22)"
  (flet ((x (e)
	   (car e))
	 (y (e)
	   (cdr e)))
    (let ((dir1 (if (= (x e11) (x e12)) :v :h))
	  (dir2 (if (= (x e21) (x e22)) :v :h)))
      (cond 
	((and (eql dir1 :h) 
	      (eql dir2 :v))
	 (if (and (in-order (x e11) (x e21) (x e12))
		  (in-order (y e21) (y e11) (y e22)))
	     (cons (x e21) (y e11))))
	((and (eql dir1 :v) 
	      (eql dir2 :h))
	 (if (and (in-order (y e11) (y e21) (y e12))
		  (in-order (x e21) (x e11) (x e22)))
	     (cons (x e11) (y e21))))))))



(defun solve1 () 
  (let ((v1 (path-to-vertices (first +input+)))
	(v2 (path-to-vertices (second +input+)))
	point 
	min)
    (loop for e21 in v2 
	  for e22 in (rest v2)do 
	    (loop for e11 in v1 
		  for e12 in (rest v1) 
		  for e = (inters e11 e12 e21 e22) do 
		    (if e 
			(print e))
		    (if e 
			(if (or (not min) (< (+ (abs (car e)) (abs (cdr e))) min))
			    (setf min (+ (abs (car e)) (abs (cdr e)))
				  point e)))))
    (values point min)))

(defun dist (e1 e2)
  (+ (abs (- (car e1) (car e2)))
     (abs (- (cdr e1) (cdr e2)))))

(defun solve2 () 
  (let ((v1 (path-to-vertices (first +input+)))
	(v2 (path-to-vertices (second +input+)))
	point 
	min)
    (loop for e21 in v2 
	  for e22 in (rest v2)
	  with step1 = (dist (cons 0 0) (first v2)) do 
	    (loop for e11 in v1 
		  for e12 in (rest v1) 
		  for e = (inters e11 e12 e21 e22) 
		  with step2 = (dist (cons 0 0) (first v1)) do 
		    (if e 
			(if (or (not min) (< (+ step1 step2 (dist e21 e) (dist e11 e))
					     min))
			    (setf min (+ step1 step2 (dist e21 e) (dist e11 e))
				  point e)))
		    (incf step2 (dist e11 e12)))
	    (incf step1 (dist e21 e22)))
    (values point min)))
	     
(defun test () 
  (let ((+input+ (list "R8,U5,L5,D3" "U7,R6,D4,L4")))
    (solve2)))

(defun test () 
  (let ((+input+ (list "R98,U47,R26,D63,R33,U87,L62,D20,R33,U53,R51" "U98,R91,D20,R16,D67,R40,U7,R15,U6,R7")))
    (solve2)))
