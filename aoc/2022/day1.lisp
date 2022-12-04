(in-package :aoc)

(defun total-calories ()
  (let ((calories (split-sequence ""
                                  (input 01 :lines)
                                  :test #'string-equal)))
    (mapcar (lambda (seq)
              (reduce #'+ seq :key #'parse-integer))
            calories)))

(defun solve1 ()
  (apply #'max (total-calories)))


(defun solve2 ()
  (reduce #'+ (bestn 3 (total-calories) #'>)))
