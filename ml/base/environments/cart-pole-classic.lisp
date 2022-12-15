(defpackage #:ml/environment/cart-pole-classic
  (:use #:cl #:ml/environment)
  (:local-nicknames (#:u #:ml/utils)))

(in-package #:ml/environment/cart-pole-classic)
(defstate pole x θ v ω)

(defmethod initialize ((e (eql :cart-pole-classic)))
  ;; a random state
  (make-pole-state :x (u:gaussian-random -0.05 0.05)
                   :v (u:gaussian-random -0.05 0.05)
                   :θ (u:gaussian-random -0.05 0.05)
                   :ω (u:gaussian-random -0.05 0.05)))

(defmethod terminated? ((e (eql :cart-pole-classic)) s)
  (let ((xlow -2.4)
        (xhigh 2.4)
        (low -12)
        (high 12))
    (or (< (pole-x s) xlow) (> (pole-x s) xhigh)
        (not (< (* low pi (/ 180))
                (pole-θ s)
                (* high pi (/ 180)))))))

(defun a-α (θ ω action)
  "calculate a i.e. d²x/dt² and α i.e. d²θ/dt² for the pole"
  (let* ((masspole 0.1)
         (masscart 1.0)
         (mass (+ masspole masscart))
         (length 0.5) ;; 1/2 of length
         (mass-length (* masspole length))
         (force (if (= action 0) -10.0 10.0))
         (sintheta (sin θ))
         (costheta (cos θ))
         (temp (/ (+ force (* mass-length (expt ω 2) sintheta))
                  mass))
         (α (/ (- (* 9.8 sintheta)
                  (* temp costheta))
               (* length (- 4/3 (/ (* masspole (expt costheta 2))
                                   mass)))))
         (a (- temp (* mass-length α costheta (/ mass)))))
    (values a α)))

(defmethod take-action ((e (eql :cart-pole-classic)) state action)
  (flet ((dS/dt (state)
           (multiple-value-bind (a α) (a-α (pole-θ state) (pole-ω state) action)
             (make-pole-state :x (pole-v state) :θ (pole-ω state)
                              :v a  :ω α))))
    (euler-forward state #'dS/dt 0.02)))
