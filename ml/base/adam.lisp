(in-package #:ml/optimizer)

;; Utility
(defmacro map-combined-arrays (arrays (index element) &body body)
  (u:with-gensyms (arr k)
  `(loop with ,index = 0
         for ,arr in ,arrays do
           (loop for ,k from 0 below (array-total-size ,arr) do
                 (symbol-macrolet ((,element
                                     (row-major-aref ,arr ,k)))
                   ,@body)
                 (incf ,index)))))

;;; Adam Optimizer
(defun adam (parameters &key
                      (alpha 0.001)
                      (beta1 0.9) (beta2 0.99)
                      (epsilon (expt 10 -8)))
  (let* ((total-size (reduce #'+ parameters :key (u:compose #'array-total-size #'t:storage)))
         (m (make-array total-size :initial-element 0.0d0))
         (v (make-array total-size :initial-element 0.0d0))
         (params (mapcar #'t:storage parameters))
         (grads (mapcar #'t:gradient-batch parameters))
         (beta1^t beta1)
         (beta2^t beta2))
    (lambda ()
      (map-combined-arrays grads (i gi)
        (setf (aref m i)
              (+ (* beta1 (aref m i)) (* (- 1 beta1) gi)))
        (setf (aref v i)
              (+ (* beta2 (aref v i)) (* (- 1 beta2) (expt gi 2))))
        (setf gi 0.0d0))
      (map-combined-arrays params (i p)
        (let ((m^ (/ (aref m i) (- 1 beta1^t)))
              (v^ (/ (aref v i) (- 1 beta2^t))))
          (setf p (- p (* alpha (/ m^ (+ (sqrt v^) epsilon))))))))))
