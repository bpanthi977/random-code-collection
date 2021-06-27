(ql:quickload :gsll)
(in-package #:fem)

(defun make-gsll-matrix (lisp-array)
  (cond ((typep  lisp-array 'grid:foreign-array)
         lisp-array)
        ((= (array-rank lisp-array) 2)
         (let ((matrix (make-instance 'grid:matrix-double-float
                                      :element-type 'double-float
                                      :dimensions (array-dimensions lisp-array))))
           (loop for i from 0 below (array-dimension lisp-array 0) do
             (loop for j from 0 below (array-dimension lisp-array 1) do
               (setf (grid:aref matrix i j) (coerce (aref lisp-array i j) 'double-float))))
           matrix))
        ((= (array-rank lisp-array) 1)
         (let ((matrix (make-instance 'grid:vector-double-float
                                      :element-type 'double-float
                                      :dimensions (array-dimensions lisp-array))))
           (loop for i from 0 below (length lisp-array) do
             (setf (grid:aref matrix i) (coerce (aref lisp-array i) 'double-float)))
           matrix))
        (t (error "Con't covert to gsll array"))))

(defun determinant (matrix)
  (multiple-value-bind (lu perm signum)
      (gsll:lu-decomposition (make-gsll-matrix matrix))
    (declare (ignore perm))
    (gsll:lu-determinant lu signum)))

(defun invert (matrix)
  (multiple-value-bind (lu perm signum)
      (gsll:lu-decomposition (make-gsll-matrix matrix))
    (declare (ignore signum))
    (gsll:lu-invert lu perm)))

(defun matmul (m1 m2 &optional alpha)
  "m1*m2 * alpha; alpha is scalar"
  (if alpha
      (gsll:matrix-product (make-gsll-matrix m1) (make-gsll-matrix m2) nil alpha)
      (gsll:matrix-product (make-gsll-matrix m1) (make-gsll-matrix m2))))

(defun solve-lineqn (A b)
  "Solves A x = b"
  (multiple-value-bind (lu perm signum)
      (gsll:lu-decomposition (make-gsll-matrix A))
    (declare (ignore signum))
    (gsll:lu-solve lu
                   (make-gsll-matrix b)
                   perm
                   t)))

(defun transpose (m1)
  (einsum (ij :to ij)
          (ji m1)))
