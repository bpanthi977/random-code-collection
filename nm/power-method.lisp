;;; Power method for finding largest eigenvalue
(load "../ml/einstein.lisp")
(defun power-method (A &key x0 tolerance steps)
  "Start with an initial guess of the eigenvector then iterate"
  ;; take a vector with all ones as initial guess
  (destructuring-bind (m n) (array-dimensions A)
    (declare (ignore m))
    (unless x0
      (setf x0 (make-array n
                           :element-type (array-element-type A)
                           :initial-element (coerce 1 (array-element-type A)))))
    (let ((x (make-array n :element-type (array-element-type A)))
          lambda
          (iteration-count 0))

      ;; iteration
      (loop do
        (incf iteration-count)
        ;; x = A x0
        (einsum::einsum (ij :to i) :into x
                        (* (ij A) (j x0)))
        ;; find maximal element
        (setf lambda (reduce #'max x))
        ;; normalize x
        (einsum::einsum (i :to i) :into x
                        (/ (i x) lambda))
        ;; check the difference with previous value
        (when (< (reduce #'max (map 'vector (lambda (a b)
                                              (abs (- a b)))
                                    x x0))
                 tolerance)
          (return))
        (when (> iteration-count steps)
          (return))
        (rotatef x x0))
      (values lambda x iteration-count))))
