(defpackage :aoc12
  (:use :cl :aoc))

(in-package :aoc12)

(defparameter *input* (input 12 :lines))

(defun parse-line (string)
  (let ((char (char string 0)))
    (cons (intern (string (char-upcase char)) :keyword)
          (parse-integer string :start 1))))

(defun solve1 ()
  (let ((input (mapcar #'parse-line *input*)))
    (loop with pos = #C(0 0)
          with dir = #C(1 0)
          for (inst . steps) in input do 
            (case inst
              (:E (incf pos (complex steps 0)))
              (:W (decf pos (complex steps 0)))
              (:N (incf pos (complex 0 steps)))
              (:S (decf pos (complex 0 steps)))
              (:F (incf pos (* steps dir)))
	      (:L (setf dir (* dir (expt (complex 0 1) (/ steps 90)))))
              (:R (setf dir (* dir (expt (complex 0 -1) (/ steps 90))))))
          finally (return (+ (abs (realpart pos))
                             (abs (imagpart pos)))))))

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
              (:F (incf pos (* steps waypoint)))
              (:L (setf waypoint (* waypoint (expt (complex 0 1) (/ steps 90)))))
              (:R (setf waypoint (* waypoint (expt (complex 0 -1) (/ steps 90))))))
            finally (return (+ (abs (realpart pos))
                               (abs (imagpart pos)))))))
