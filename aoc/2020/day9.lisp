(defpackage :aoc9
  (:use :cl :aoc))

(in-package :aoc9)

(defparameter *input* (map 'vector #'parse-integer (input 9 :lines)))

(defun check-valid? (input i)
  (loop for j from (max 0 (- i 25)) below i
        with n = (aref input i)
        when (find (- n (aref input j)) input :start j :end i)
          return t))

(defun solve1 () 
  (loop for i from 25 below (length *input*) 
        unless (check-valid? *input* i)
          return (aref *input* i)))
                                          
(defun solve2% (input index target) 
  (loop for i from index below (length input)
        summing (aref input i) into total
        when (= total target)
          return (+ (reduce #'max input :start index :end i)
                    (reduce #'min input :start index :end i))
        when (> total target)
          return nil))

(defun solve2 (target) 
  (loop for i from 0 below (length *input*)
        for s = (solve2% *input* i target)
        when s
          return s))

(defun solve2* (target) 
  (macrolet ((move-front () 
               `(progn 
                  (incf j)
                  (when (= j length) (return nil))
                  (incf sum (aref *input* j))))
             (move-back () 
               `(progn 
                  (decf sum (aref *input* i))
                  (incf i))))
    (loop with sum = (aref *input* 0)
          with i = 0 
          with j = 0 
          with length = (length *input*) do 
            (cond ((= target sum)
                   (return (+ (reduce #'max *input* :start i :end j)
                              (reduce #'min *input* :start i :end j))))
                  ((> target sum) 
                   (move-front))
                  ((< target sum)
                   (when (= i j) (move-front))
                   (move-back))))))
