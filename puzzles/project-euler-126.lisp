;;; https://projecteuler.net/problem=126

(defun c1 (x k)
  "Number of cubes in 1D
When we start with `x' cubes and iterate `k' times.

At each step we add 1 cube on each side of the line."
  (+ x (* 2 k)))

(defun del1-0 (x k)
  (- (c1 x k)
     (c1 x (- k 1))))

(defun del1 (x k)
  (declare (ignore x k))
  2)

(defun c2 (x y k)
  "Number of cubes in 2D
When we start with a grid of `x' by `y' cubes and iterate `k' times

At each step we increase the y lines with 1 cube on each side,
and add a two new lines of size x.
"
  (+ (* y (c1 x k))
     (* 2 (loop for i from 0 below k
                summing (c1 x i)))))

(defun del2-0 (x y k)
  (- (c2 x y k)
     (c2 x y (- k 1))))

(defun del2-1 (x y k)
  (+ (* y (del1 x k))
     (* 2 (c1 x (- k 1)))))

(defun del2 (x y k)
  (+ (* y 2)
     (* 2 (+ x (* 2 (- k 1))))))

(defun c3 (x y z k)
  "Number of cubes in 3D
When we start with a grid of `x' by `y' by `z' cubes and iterate `k' times"
  (+ (* z (c2 x y k))
     (* 2 (loop for i from 0 below k
                summing (c2 x y i)))))

(defun del3-0 (x y z k)
  "Number of cubes in the k-th layer
when we start with a grid of `x' by `y' by `z'"
  (- (c3 x y z k)
     (c3 x y z (- k 1))))

(defun del3 (x y z k)
  (+ (* z (del2 x y k))
     (* 2 (c2 x y (- k 1)))))

(defun solve126-check (target)
  "Number of cuboids that contain `target' number of cubes in one of their layers"
  (let ((count 0))
    (flet ((solve-z (del c2)
             "Does integer solution of `z' exits for z * del + 2 * c2 =  target"
             (let ((diff (- target (* 2 c2))))
               (if (and (> diff 0)
                        (= (mod diff del) 0))
                   (/ diff del)
                   0))))
      (loop for x from 1 below target do
        (loop for y from x below (floor target x) do
          (loop for k from 1
                with prev_c2 = (* x y)
                for del2 = (del2 x y k) do
                  (when (> prev_c2 target)
                    (return))
                  (let ((z (solve-z del2 prev_c2)))
                    (when (and (not (zerop z))
                               (>= z y))
                      (incf count 1)))
                  (setf prev_c2 (+ prev_c2 del2)))))
      count)))

(defun solve126-0 (req-count)
  "Bruteforce solution"
  (loop for diff from 1
        for count = (solve126-check diff)
        when (= count req-count)
          do (return diff)))

(defun test-given-data ()
  (assert (= 6 (c3 3 2 1 0)))
  (assert (= 22 (del3 3 2 1 1)))
  (assert (= 46 (del3 3 2 1 2)))
  (assert (= 78 (del3 3 2 1 3)))
  (assert (= 118 (del3 3 2 1 4)))

  (assert (= 22 (del3 5 1 1 1)))

  (assert (= 46 (del3 5  3 1 1)))
  (assert (= 46 (del3 7  2 1 1)))
  (assert (= 46 (del3 11 1 1 1)))

  (assert (= (solve126-0 10) 154)))

(defun test-equal (f1 f2 vars)
  (loop repeat 1000
        for args = (loop repeat (+ 1 vars) collect (+ 1 (random 10))) do
          (assert (= (apply f1 args)
                     (apply f2 args))
                  nil
                  "Test failed at ~a ~%v1=~a v2=~a"
                  args
                  (apply f1 args) (apply f2 args))))

(defun test-helpers ()
  (test-equal #'del1-0 #'del1 1)
  (test-equal #'del2-0 #'del2 2)
  (test-equal #'del3-0 #'del3 3))

;;;; THE SOLUTION

(defun del2 (x y k)
  "For 2D case, the number of cubes added in k-th layer
when we start with a grid of `x' by `y' cubes"
  (+ (* y 2)
     (* 2 (+ x (* 2 (- k 1))))))

(defun solve126 (target max-dim max-del)
  (let ((counts (make-array max-del :element-type 'fixnum)))

    (loop for x from 1 below max-dim do
      (loop for y from x below max-dim do
        (loop named block-k
              for k from 1
              with prev_c2 = (* x y)
              for del2 = (del2 x y k) do
                (loop named block-z
                      for z from y
                      for del = (+ (* z del2)
                                   (* 2 prev_c2))
                      do
                         (when (>= del max-del)
                           (if (= z y)
                               (return-from block-k)
                               (return-from block-z)))
                         (incf (aref counts del)))
                (setf prev_c2 (+ prev_c2 del2)))))
    (loop for i from 0
          for c across counts
          when (= c target) do (return i))))

;; (solve126 1000 100 19000)
