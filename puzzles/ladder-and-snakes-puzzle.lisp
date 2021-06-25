;; https://www.youtube.com/watch?v=N3JL3z4e2Qs

(ql:quickload :screamer)
(in-package :screamer-user)

(defparameter *jumps* '((4 75) (5 15) (19 41) (28 50)
                        (35 96) (44 82) (53 94) (59 95)
                        (70 91)
                        (98 12) (88 67) (81 62) (76 41)
                        (52 23) (47 30) (31 8) (21 3)))

(defun make-move (position)
  (let* ((choice (screamer:a-member-of '(1 2 3 4 5 6)))
         (new-position (+ position choice))
         (jump (find new-position *jumps* :key #'car)))
    (if jump
        (values (second jump) jump)
        (values position position))))

;; (expt 6 5) = 7776 isn't much so we can bruteforce a solution
(defun solve (position remaining-moves)
  (cond ((= position 100)
         nil)
        ((= remaining-moves 0)
         (fail))
        (t
         (multiple-value-bind (new-position move) (make-move position)
           (cons move
                 (solve new-position (1- remaining-moves)))))))

(defun solve-both ()
  (let* ((soln (one-value (solve 0 5)))
         (jumps (set-difference *jumps* soln :test #'equal)))

    (values soln
            (let ((*jumps* jumps))
              (one-value (solve 0 5))))))

(solve-both)
