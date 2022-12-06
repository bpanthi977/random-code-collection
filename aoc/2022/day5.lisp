(in-package :aoc)
(defun parse-integers (string)
  (let (numbers n)
    (loop for char across string do
          (cond
            ((digit-char-p char)
             (unless n (setf n 0))
             (setf n (+ (* n 10) (- (char-code char) #.(char-code #\0)))))
            (t
             (when n
               (push n numbers)
               (setf n nil)))))
    (when n (push n numbers))
    (reverse numbers)))

(defun parse-stacks (lines)
  (let (stacks)
    (loop for line in lines
          for crates = (loop for i from 1 below (length line) by 4
                             collect (char line i))
          do
             (unless stacks
               (setf stacks (make-array (length crates) :initial-element nil)))
             (loop for i from 0
                   for crate in crates do
                     (unless (char= crate #\Space)
                       (push crate (aref stacks i)))))
    (map 'vector #'nreverse stacks)))

(defun solve1 ()
  (let* ((lines (input 05 :lines))
         (pos (position "" lines :test #'string-equal))
         (stacks (parse-stacks (subseq lines 0 pos))))
    (loop for move in (subseq lines (1+ pos))
          for (n from to) = (parse-integers move) do
            (loop repeat n do
              (setf (aref stacks (1- to)) (cons (car (aref stacks (1- from)))
                                                (aref stacks (1- to)))
                    (aref stacks (1- from)) (cdr (aref stacks (1- from))))))
    (map 'string #'first stacks)))

(defun solve2 ()
  (let* ((lines (input 05 :lines))
         (pos (position "" lines :test #'string-equal))
         (stacks (parse-stacks (subseq lines 0 pos))))
    (loop for move in (subseq lines (1+ pos))
          for (n from to) = (parse-integers move) do
            (let* ((stack-head (aref stacks (1- from)))
                   (split-cons (nthcdr (1- n) stack-head)))
              (setf (aref stacks (1- from)) (cdr split-cons)
                    (cdr split-cons) (aref stacks (1- to))
                    (aref stacks (1- to)) stack-head)))
    (map 'string #'first stacks)))
