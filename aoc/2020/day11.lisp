(defpackage :aoc11
  (:use :cl :aoc))

(in-package :aoc11)

(defparameter *input* (input 11 :lines))

(defun parse-input (input)
  (let ((grid (make-array (list (length input)
                                (length (first input)))
                          :element-type '(member :floor :empty :occupied)
                          :initial-element :floor)))
    (loop for row in input
          for i from 0 do 
          (loop for cell across row
                for j from 0 do 
                (setf (aref grid i j)
                      (case cell 
                        (#\. :floor) (#\L :empty) (#\# :occupied)
                        (t (error "Unknown cell type"))))))
    grid))

(defun dir-occupiedp (grid i j dirx diry)
  (when (array-in-bounds-p grid (+ i dirx) (+ j diry))
    (eql (aref grid (+ i dirx) (+ j diry)) :occupied)))

(defun count-if-in-dir (predicate grid m n)
  (let ((count 0))
    (loop for dirx from -1 to 1 do
      (loop for diry from -1 to 1 do
        (unless (= dirx diry 0)
          (when (funcall predicate grid m n dirx diry)
            (incf count)))))
    count))

(defun evolve1 (grid i j)
  (let ((oc (count-if-in-dir #'dir-occupiedp grid i j))
        (cell (aref grid i j)))
    (cond ((and (eql cell :empty)
                (= oc 0))
           :occupied)
          ((and (eql cell :occupied)
                (>= oc 4))
           :empty)
          (t cell))))

(defun map-grid-into (newgrid grid function)
  (destructuring-bind (m n) (array-dimensions grid)
    (loop for i from 0 below m do
          (loop for j from 0 below n do 
                (setf (aref newgrid i j)
                      (funcall function grid i j)))))
  newgrid)

(defun grid-equal (grid newgrid)
  (loop for i from 0 below (array-total-size grid)
        unless (eql (row-major-aref grid i)
                    (row-major-aref newgrid i))
          return nil
        finally (return t)))

(defun solve (evolution-function &optional (input *input*))
  (let* ((grid (parse-input input))
         (newgrid (make-array (array-dimensions grid) :initial-element :empty)))
    (loop do 
      (map-grid-into newgrid grid evolution-function)
      (when (grid-equal grid newgrid)
        (return))
      (rotatef grid newgrid))
    (count :occupied (make-array (array-total-size grid) :displaced-to grid))))

(defun solve1 ()
  (solve #'evolve1))

(defun dir-occupiedp2 (grid i j dirx diry)
  (let ((newi (+ i dirx))
        (newj (+ j diry)))
    (when (array-in-bounds-p grid newi newj)
      (case (aref grid newi newj)
        (:empty nil)
        (:occupied t)
        (:floor (dir-occupiedp2 grid newi newj dirx diry))))))

(defun evolve2 (grid i j)
  (let ((oc (count-if-each-dir #'dir-occupiedp2 grid i j))
        (cell (aref grid i j)))
    (cond ((and (eql cell :empty)
                (= oc 0))
           :occupied)
          ((and (eql cell :occupied)
                (>= oc 5))
           :empty)
          (t cell))))

(defun solve2 ()
  (solve #'evolve2))


