;; https://projecteuler.net/problem=91


(declaim (inline dot))
(defun dot (ax ay bx by cx cy)
  (declare (type fixnum ax ay bx by cx cy))
  (the fixnum
       (+ (the fixnum (* (- ax bx) (- bx cx)))
          (the fixnum (* (- ay by) (- by cy))))))

(defun is-right-angled (px py qx qy)
  (declare (type fixnum px py qx qy))
  (or (= (dot px py qx qy  0  0) 0)
      (= (dot qx qy  0  0 px py) 0)
      (= (dot  0  0 px py qx qy ) 0)))

(defun is-degenerate (px py qx qy)
  (declare (type fixnum px py qx qy))
  (or (and (= px qx) (= py qy))
      (= px py 0)
      (= qx qy 0)))

(defun solve (max)
  (let ((count 0))
    (declare (type fixnum count))
    (loop for px fixnum from 0 to max do
          (loop for py from (if (= px 0) 1 0) to max do
                (loop for qx from  0 to max do
                      (loop for qy from (if (= qx 0) 1 0) to max
                            for q = (cons qx qy)
                            for p = (cons px py) do
                            (when (and (is-right-angled px py qx qy)
                                       (not (is-degenerate px py qx qy)))
                              (incf count))))))
    (/ count 2)))
