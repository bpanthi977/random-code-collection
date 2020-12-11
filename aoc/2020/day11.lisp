(defpackage :aoc11
  (:use :cl :aoc))

(in-package :aoc11)

(defparameter *input* (input 11 :lines))

;;; Utilities
(defun map-grid-into (newgrid grid function)
  "map `function' to each element of 2d `grid' and store in `newgrid'"
  (destructuring-bind (m n) (array-dimensions grid)
    (loop for i from 0 below m do
          (loop for j from 0 below n do 
                (setf (aref newgrid i j)
                      (funcall function grid i j)))))
  newgrid)

(defun grid-equalp (grid newgrid)
  (loop for i from 0 below (array-total-size grid)
        unless (eql (row-major-aref grid i)
                    (row-major-aref newgrid i))
          return nil
        finally (return t)))

(defun dir-count-if (predicate grid m n)
  "for each 8 directions from position (m,n) of `grid' count if `predicate' returns true"
  (let ((count 0))
    (loop for dirx from -1 to 1 do
      (loop for diry from -1 to 1 do
        (unless (= dirx diry 0)
          (when (funcall predicate grid m n dirx diry)
            (incf count)))))
    count))

;;; Parsing
(defun parse-input (input)
  (let ((array (make-array (list (length input)
                                 (length (first input)))
                           :initial-contents input)))
    (dotimes (i (array-total-size array))
      (setf (row-major-aref array i)
            (case (row-major-aref array i)
              (#\. :floor) (#\L :empty) (#\# :occupied)
              (t (error "Unknown cell type")))))
    array))

;;; Solution part 1
(defun occupiedp (grid i j dirx diry)
  (when (array-in-bounds-p grid (+ i dirx) (+ j diry))
    (eql (aref grid (+ i dirx) (+ j diry)) :occupied)))

(defun evolve1 (grid i j)
  (let ((oc (dir-count-if #'occupiedp grid i j))
        (cell (aref grid i j)))
    (cond ((and (eql cell :empty)
                (= oc 0))
           :occupied)
          ((and (eql cell :occupied)
                (>= oc 4))
           :empty)
          (t cell))))

(defun evolve-till-stable (evolution-function grid)
  (let ((newgrid (make-array (array-dimensions grid))))
    (loop do 
      (map-grid-into newgrid grid evolution-function)
      (when (grid-equalp grid newgrid)
        (return))
      (rotatef grid newgrid))
    newgrid))

(defun solve1 ()
  (let* ((grid (parse-input *input*))
         (solution (evolve-till-stable #'evolve1 grid)))
    (count :occupied (make-array (array-total-size solution) :displaced-to solution))))

;;; Solution part 2
(defun dir-occupiedp (grid i j dirx diry)
  (let ((newi (+ i dirx))
        (newj (+ j diry)))
    (when (array-in-bounds-p grid newi newj)
      (case (aref grid newi newj)
        (:empty nil)
        (:occupied t)
        (:floor (dir-occupiedp grid newi newj dirx diry))))))

(defun evolve2 (grid i j)
  (let ((oc (dir-count-if #'dir-occupiedp grid i j))
        (cell (aref grid i j)))
    (cond ((and (eql cell :empty)
                (= oc 0))
           :occupied)
          ((and (eql cell :occupied)
                (>= oc 5))
           :empty)
          (t cell))))

(defun solve2 ()
  (let* ((grid (parse-input *input*))
         (solution (evolve-till-stable #'evolve2 grid)))
    (count :occupied (make-array (array-total-size solution) :displaced-to solution))))


