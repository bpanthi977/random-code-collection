(in-package :aoc)

(defun parse-integers (n string &key (start 0))
  (if (= 0 n)
      nil
      (multiple-value-bind (int pos)
          (parse-integer string :start start :junk-allowed t)
        (cons int (parse-integers (1- n) string :start (1+ pos))))))

(defun fully-contatined? (line)
  (destructuring-bind (a1 b1 a2 b2)
      (parse-integers 4 line)
    (or (<= a1 a2 b2 b1) ;; first contains second
        (<= a2 a1 b1 b2)))) ;; second contains first

(defun solve1 ()
  (count-if #'fully-contatined? (input 04 :lines)))

(defun overlaps? (line)
  (destructuring-bind (a1 b1 a2 b2)
      (parse-integers 4 line)
    (or (<= a1 a2 b1)
        (<= a2 a1 b2))))

(defun solve2 ()
  (count-if #'overlaps? (input 04 :lines)))
