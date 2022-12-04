(in-package :aoc)

(defun misplaced-item (line)
  (multiple-value-bind (a b) (halves line)
    (find-if (lambda (x)
               (find x b :test #'char=))
             a)))

(defun priority (char)
  (let ((code (char-code char)))
    (cond ((< code #.(char-code #\a))
           (+ 27 (- code #.(char-code #\A))))
          (t (+ 1 (- code #.(char-code #\a)))))))

(defun solve1 ()
  (reduce #'+ (input 03 :lines) :key (compose #'priority #'misplaced-item)))

(defun badge (lines)
  (destructuring-bind (a b c) lines
    (find-if (lambda (x)
               (and (find x b :test #'char=)
                    (find x c :test #'char=)))
             a)))

(defun solve2 ()
  (reduce #'+ (batches (input 03 :lines) 3)
          :key (compose #'priority #'badge)))
