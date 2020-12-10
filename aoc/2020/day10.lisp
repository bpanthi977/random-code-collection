(defpackage :aoc10
  (:use :cl :aoc))

(in-package :aoc10)

(defparameter *input* (map 'vector #'parse-integer (input 10 :lines)))

(defun solve1 (&optional (input *input*))
  (loop for k across (sort input #'<)
        with one = 0 
        with three = 1 
        with prev = 0 do 
          (case (- k prev)
            (1 (incf one))
            (3 (incf three)))
          (setf prev k)
        finally (return (* one three))))

(defun solve2% (index input memoize)
  (when (gethash index memoize)
    (return-from solve2% (gethash index memoize)))
  (setf (gethash index memoize)
        (let ((lasti (1- (length input)))
              (val (aref input index)))
          (cond ((= index lasti)
                 1)
                (t (loop for i from (1+ index) to lasti
                         for v = (aref input i) 
                         unless (<= v (+ val 3))
                           return sum
                         summing (solve2% i input memoize) into sum
                         finally (return sum)))))))

(defun solve2 (&optional (input *input*))
  (let ((sorted (sort input #'<))
        (memoize (make-hash-table :size (length input))))
    (solve2% 0 
             (concatenate 'vector #(0) sorted)
             memoize)))
