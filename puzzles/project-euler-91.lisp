;; https://projecteuler.net/problem=91

(defun dot (a b)
  (+ (* (car a) (car b))
     (* (cdr a) (cdr b))))


(defun diff (a b)
  (cons (- (car a) (car b))
        (- (cdr a) (cdr b))))

(defun is-right-angled (a b c)
  (or (= (dot (diff a b) (diff b c)) 0)
      (= (dot (diff b c) (diff c a)) 0)
      (= (dot (diff c a) (diff a b)) 0)))

(defun is-degenerate (a b c)
  (or (equal a b)
      (equal b c)
      (equal c a)))

(defun solve (max)
  (let ((count 0)
        (origin (cons 0 0)))
    (loop for px from 0 to max do
          (loop for py from (if (= px 0) 1 0) to max do
                (loop for qx from  0 to max do
                      (loop for qy from (if (= qx 0) 1 0) to max
                            for q = (cons qx qy)
                            for p = (cons px py) do
                            (when (and (is-right-angled origin p q)
                                       (not (is-degenerate origin p q)))
                              (incf count))))))
    (/ count 2)))
