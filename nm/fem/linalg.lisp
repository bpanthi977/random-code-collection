(in-package #:fem)

(defun minor (matrix i j)
  (let* ((n (array-dimension matrix 0))
         (minor (make-array (list (1- n) (1- n)))))
    (loop for i* from 0 below (1- n) do
      (loop for j* from 0 below (1- n) do
        (setf (aref minor i* j*)
              (aref matrix
                    (if (< i* i) i* (1+ i*))
                    (if (< j* j) j* (1+ j*))))))
    minor))

(defun determinant (matrix)
  (let ((n (array-dimension matrix 0)))
    (cond ((= n 0) (error "No Determinant of a zero dimensional matrix"))
          ((= n 1) (aref matrix 0 0))
          ((= n 2 (- (* (aref matrix 0 0) (aref matrix 1 1))
                     (* (aref matrix 0 1) (aref matrix 1 0)))))
          (t
           (loop for j from 0 below n
                 summing (* (aref matrix 0 j)
                            (expt -1 j)
                            (determinant (minor matrix 0 j))))))))

(defun adjoint (matrix)
  (let* ((n (array-dimension matrix 0))
         (adjoint (make-array (list n n))))
    (loop for i from 0 below n do
      (loop for j from 0 below n do
        (setf (aref adjoint i j)
              (* (expt -1 (+ i j))
                 (determinant (minor matrix i j))))))
    adjoint))

(defun invert (matrix)
  (let ((det (determinant matrix)))
    (when (= det 0)
      (error "Determinant of matrix is zero; can't invert"))
    (let ((adj (adjoint matrix)))
      (einsum (ij :to ij)
              (/ (ji adj) det)))))

(defun matmul (m1 m2 &optional (alpha 1))
  (cond ((vectorp m2)
         (einsum (ij :to i)
                 (* (ij m1) (j m2) alpha)))
        (t
         (einsum (ijk :to ik)
                 (* (ij m1) (jk m2) alpha)))))

(defun solve-lineq (A b)
  "Solves A x = b"
  (matmul (invert A) b))

(defun transpose (m1)
  (einsum (ij :to ij)
          (ji m1)))
