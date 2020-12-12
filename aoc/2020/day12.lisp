(defpackage :aoc12
  (:use :cl :aoc))

(in-package :aoc12)

(defparameter *input* (input 12 :lines))

(defun parse-line (string)
  (let ((char (char string 0)))
    (cons (intern (string (char-upcase char)) :keyword)
	  (parse-integer string :start 1))))

(defun right (current-dir angles)
  (cond ((= angles 90)
	 (case current-dir
	   (:N :E)
	   (:E :s)
	   (:s :w)
	   (:w :n)))
	((= angles 180)
	 (right (right current-dir 90) 90))
	((= angles 270)
	 (right (right current-dir 90) 180))))

(defun left (current-dir angle)
  (right current-dir (- 360 angle)))


(defun solve1 ()
  (let ((input (mapcar #'parse-line *input*)))
    (loop with mydir = :E
	  with posx = 0
	  with posy = 0
	  for (dir . steps) in input do 
	    (print (list dir steps))
	    (tagbody moveindir 
	      (case dir
		(:E (incf posx steps))
		(:W (decf posx steps))
		(:N (incf posy steps))
		(:S (decf posy steps))
		(:F (setf dir mydir)
		 (go moveindir))
		(:R (setf mydir (right mydir steps)))
		(:L (setf mydir (left mydir steps)))))
	    (print (list mydir posx posy))

	  finally (return (+ (abs posx) (abs posy))))))
	    

(defun solve2 ()
  (let ((input (mapcar #'parse-line *input*)))
    (loop with waypoint = #C(10 1)
	  with pos = #C(0 0)
	  for (dir . steps ) in input do
	    (case dir
	      (:E (incf waypoint (complex steps 0)))
	      (:W (decf waypoint (complex steps 0)))
	      (:N (incf waypoint (complex 0 steps)))
	      (:S (decf waypoint (complex 0 steps)))
	      (:F (loop repeat steps do (incf pos waypoint)))
	      (:L (loop repeat (/ steps 90) do (setf waypoint (* waypoint (complex 0 1)))))
	      (:R (loop repeat (/ steps 90) do (setf waypoint (* waypoint (complex 0 -1))))))
	    finally (return (+ (abs (realpart pos))
			       (abs (imagpart pos)))))))
		 
