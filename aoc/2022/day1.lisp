(in-package :aoc)

(defun solve1 ()
  (let ((totals (mapcar (lambda (seq)
                          (reduce #'+ seq :key #'parse-integer))
                        (split-sequence ""
                                        (input 01 :lines)
                                        :test #'string-equal))))
    (apply #'max totals)))


(defun solve2 ()
  (let ((totals (mapcar (lambda (seq)
                          (reduce #'+ seq :key #'parse-integer))
                        (split-sequence ""
                                        (input 01 :lines)
                                        :test #'string-equal))))
    (reduce #'+ (take 3 (sort totals #'>)))))
