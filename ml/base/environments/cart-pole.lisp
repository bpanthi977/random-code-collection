(defpackage #:ml/environment/cart-pole
  (:use #:cl #:ml/environment))
(in-package #:ml/environment/cart-pole)

(defstate pole x θ v ω)

(defmethod initialize ((e (eql :cart-pole)))
  ;; a random state
  (make-pole-state :x (- (random 1.0) 0.5) :θ (* pi (/ 180) (serapeum:random-in-range 70 110))))

(defmethod terminated? ((e (eql :cart-pole)) s)
  (let ((low 5)
        (high 175))
    (or (> (abs (pole-x s)) 5)
        (not (< (* low pi (/ 180))
                (pole-θ s)
                (* high pi (/ 180)))))))

;;; Physics

(defun a-α (θ base-acceln)
  "calculate a i.e. d²x/dt² and α i.e. d²θ/dt² for the pole"
  (let* ((weight 0.5)
         (length 1)
         (a (- 1 (* 1/8 (expt length 2) (sin (* 2 θ)) (cos θ))))
         (b (* 1/2 (sin θ)))
         (c (* weight (cos θ) 1/2 length))
         (d (* (sin (* 2 θ)) 1/4 length))
         (α (/ (- (* b base-acceln) c)
               (+ a (* b d))))
         (x.. (- base-acceln (* d α))))
    (values x.. α)))

(defmethod take-action ((e (eql :cart-pole)) state action)
  (flet ((dS/dt (state)
           (multiple-value-bind (a α) (a-α (pole-θ state) action)
             (make-pole-state :x (pole-v state) :θ (pole-ω state)
                              :v a  :ω α))))
    (rk4 state #'dS/dt 0.02)))

;; cart pole 2
;; -1 or +1 force only
(defmethod initialize ((e (eql :cart-pole2)))
  (initialize :cart-pole))

(defmethod terminated? ((e (eql :cart-pole2)) s)
  (terminated? :cart-pole s))

(defmethod take-action ((e (eql :cart-pole2)) state action)
  (let ((acceln (if (= 0 action) -1 1)))
    (flet ((dS/dt (state)
             (multiple-value-bind (a α) (a-α (pole-θ state) acceln)
               (make-pole-state :x (pole-v state) :θ (pole-ω state)
                           :v a  :ω α))))
      (rk4 state #'dS/dt 0.02))))
