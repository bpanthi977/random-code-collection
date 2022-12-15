(in-package #:ml/optimizer)

(defun sgd (tensors alpha)
  (loop for tensor in tensors
        for s = (t:storage tensor)
        for gb = (t:gradient-batch tensor)
        for size = (reduce #'* (array-dimensions s)) do
          (loop for i from 0 below size do
            (decf (row-major-aref s i)
                  (* alpha (row-major-aref gb i)))
            (setf (row-major-aref gb i) 0.0))))
