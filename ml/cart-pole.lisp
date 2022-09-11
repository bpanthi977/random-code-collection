;;;; cart-pole.lisp
(ql:quickload '(#:alexandria #:serapeum))
(uiop:define-package #:cart-pole/utils
    (:use #:alexandria #:serapeum)
  (:reexport #:alexandria #:serapeum))

(defpackage #:cart-pole
  (:use #:cl)
  (:local-nicknames (#:u #:cart-pole/utils)))

(in-package #:cart-pole)

(setf *read-default-float-format* 'double-float)
(declaim (optimize (speed 3) (space 0 ) (safety 0) (debug 0)))
;;; state
(defmacro defstate (&rest components)
  `(progn
     (defun make-state (&key ,@(loop for c in components
                                     collect (list c 0.0d0)))
       (vector ,@components))

     ,@(loop for c in components
             for i from 0
             collect `(defun ,c (state)
                        (elt state ,i)))))

(defun state+  (state1 state2)
  (map 'vector #'+ state1 state2))

(defun state* (k state)
  (map 'vector (lambda (component)
                 (* k component))
       state))

(defstate x θ v ω)

;;; Configuration

(defparameter *l* 2)
(defparameter *w* 0.5)
(defparameter *δt* 0.2)

;;; Physics

(defun a-α% (θ base-acceln)
  "calculate a i.e. d²x/dt² and α i.e. d²θ/dt² for the pole"
  (let* ((a (- 1 (* 1/8 (expt *l* 2) (sin (* 2 θ)) (cos θ))))
         (b (* 1/2 (sin θ)))
         (c (* *w* (cos θ) 1/2 *l*))
         (d (* (sin (* 2 θ)) 1/4 *l*))
         (α (/ (- (* b base-acceln) c)
               (+ a (* b d))))
         (x.. (- base-acceln (* d α))))
    (values x.. α)))

(defparameter *base-acceln* 0.0) ;; m/s²
(defun a-α (state)
  (a-α% (θ state) *base-acceln*))

(defun dS/dt (state)
  "State = (θ x ω v)"
  (multiple-value-bind (a α) (a-α state)
    (make-state :x (v state) :θ (ω state)
                :v a  :ω α)))

;;; Integrator

(defun euler-forward (state Δt)
  (state+ (state* Δt (dS/dt state))
          state))

(defun rk4 (state Δt)
  (let* ((k₁ (dS/dt state))
         (k₂ (dS/dt (state+ state (state* (/ Δt 2) k₁))))
         (k₃ (dS/dt (state+ state (state* (/ Δt 2) k₂))))
         (k₄ (dS/dt (state+ state (state* Δt k₃))))
         (k (state* (/ Δt 6) (reduce #'state+
                                     (list k₁
                                           (state* 2 k₂)
                                           (state* 2 k₃)
                                           k₄)))))
    (state+ state k)))

(defun physics-update (state Δt base-acceln)
  "We update the state using RK4. We could also use euler forward but that would be much inaccurate"
  (let ((*base-acceln* (+ (* 0.1 base-acceln) (- (random 0.0002) 0.0001))))
    (rk4 state Δt)))

;;;; Learning

;;; Discretize the space
(defconstant +low+ 30)
(defconstant +high+ 150)
(defconstant +resolution+ 3)
(defconstant +xs+ 21)
(defconstant +θs+ (1+ (/ (- +high+ +low+) +resolution+)))
(defconstant +vs+ 21)
(defconstant +ωs+ 21)
(defconstant +total-actions+ 21)
(defparameter +total-states+ (* +θs+ +xs+ +vs+ +ωs+))

(defun terminate? (s)
  (or (> (abs (x s)) 1)
      (not (< #.(* +low+ pi (/ 180))
              (θ s)
              #.(* +high+ pi (/ 180))))))

(defparameter *log* (vector 0 0 0 0))

(defun sigmoid (x)
  (/ 1 (1+ (exp (- x)))))

(defun discretize-state (s)
  (if (terminate? s)
      0
      (let ((xi (+ 10 (truncate (x s) 0.1)))
            (θi (floor (- (* 180 (/ pi) (θ s)) +low+) +resolution+))
            (vi (+ 10 (floor (* 10 (sigmoid (v s))))))
            (ωi (+ 10 (floor (* 10 (sigmoid (ω s)))))))
        (+ (* +θs+ +vs+ +ωs+ xi)
           (*      +vs+ +ωs+ θi)
           (*           +ωs+ vi)
           (*                ωi)))))


;;; Q Table
(defparameter *q-table* nil)
(defparameter *q-table-touches* nil)
(defparameter *transition-table* nil)
(defparameter *discount-rate* 1)
(defparameter *iterations* 0)

(defun initialize-q-table ()
  (setf *iterations* 0)
  (setf *q-table* (make-array (* +total-actions+ +total-states+) :element-type 'double-float))
  (setf *q-table-touches* (make-array (* +total-actions+ +total-states+) :element-type 'fixnum))
  (map-into *q-table* (lambda (k)
                        (random 2.0))
            *q-table*)
  ;; terminal state value
  (loop for a from 0 to +total-actions+ do
    (setf (aref *q-table* a) 0.0)))

(defun Q (s a)
  (aref *q-table* (+ (* (discretize-state s) +total-actions+) a)))

(defun (setf Q) (value s a)
  (let ((idx (+ (* (discretize-state s) +total-actions+) a)))
    (incf (aref *q-table-touches* idx))
    (setf (aref *q-table* idx) value)))


;;; Optimal Actions and Rewards
(defun optimal-action (s)
  (let ((idx (* +total-actions+ (discretize-state s)))
        (max -1.0)
        (max-a 0))
    (loop for a from 0 below +total-actions+
          for q = (aref *q-table* (+ idx a)) do
            (when (> q max)
              (setf max q
                    max-a a)))
    (values max-a max)))

(let ((actions (u:iota +total-actions+)))
  (defun ϵ-optimal-action (s &optional (ϵ 0.5))
    (if (< (random 1.0) ϵ)
        (u:random-elt actions)
        (optimal-action s))))

(defun value (s)
  (nth-value 1 (optimal-action s)))

(defun reward (state)
  1)

;;; Play and Learn
(defun play-and-learn (state &optional (α 0.2) (δt *δt*))
  (if (terminate? state)
     0.0
     (let* ((action    (ϵ-optimal-action state))
            (new-state (physics-update state δt (- action (/ (- +total-actions+ 1) 2))))
            (reward    (reward new-state)))
       (incf (q state action)
             (* α (+ reward
                     (* *discount-rate* (play-and-learn new-state α δt))
                     (- (q state action))))))))

(defun play (initial-state &optional (δt *δt*) render-function)
  (loop with state = initial-state
        for action = (ϵ-optimal-action state 0.01)
        for new-state = (physics-update state δt (- action (/ (- +total-actions+ 1) 2)))
        for reward  = (reward new-state)
        for time from 0 by δt
        for total-reward = reward then (+ total-reward reward) do
          (when render-function (funcall render-function (x state) (θ state)))
          ;;(format t "~&x=~,4f θ=~,1f | R=~,2f A=~d, Q=~,3f" (x state) (* (θ state) 180 (/ pi)) reward action (q state action))
          (setf state new-state)
          (when (terminate? new-state)
            (return (values time total-reward)))))

(defun human-play (initial-state &optional (δt *δt*))
  (format t "~&~%")
  (format t "~&x=~,4f θ=~,1f" (x initial-state) (* (θ initial-state) 180 (/ pi)))
  (loop for action = (progn (format t "~&Your action: (0 to 20): ") (read))
        with state = initial-state
        for new-state = (physics-update state δt (- action (/ (- +total-actions+ 1) 2)))
        for reward  = (reward new-state)
        for time from 0 by δt
        for total-reward = reward then (+ total-reward reward) do
          (format t "~&x=~,4f θ=~,1f | R=~,2f A=~d, Q=~,3f" (x state) (* (θ state) 180 (/ pi)) reward action (q state action))
          (setf state new-state)
          (when (terminate? new-state)
            (return (values time total-reward)))))

(defun performance (n initial-state &optional (δt *δt*))
  (loop repeat n
        with tt = 0
        with rw = 0
        with f = 1
        do (multiple-value-bind (time reward) (play initial-state δt)
             (incf tt time)
             (incf rw (* reward f))
             (setf f (* f *discount-rate*)))
        finally (format t "~&Avg. Time:~,2f s, Avg. Reward ~,2f" (/ tt n) (/ rw n))))

(defparameter *initial-state* (make-state :x 0 :θ (* 90 pi (/ 180))))

(defun a-random-state ()
  (make-state :x (- (random 2.0) 1.0) :θ (* pi (/ 180) (+ +low+ (random (- +high+ +low+))))))

(defun learn ()
  (loop do
    (performance 1000 *initial-state*)
    (loop repeat 10000 do (play-and-learn (a-random-state))
          (incf *iterations*))))

;;; UI
(in-package :clutter)
(defclass cart-pole-app (view)
  ())

(defparameter *state* (observable (list 0 (/ pi 2))))
(defmethod build ((w cart-pole-app))
  (observe *state*
    (destructuring-bind (x theta time) (value *state*)
      (svg (cart-pole-svg x theta :time time)))))

(defun cart-pole-svg (x theta &key (scale 50) (time 0))
  (flet ((trx (x)
           (+ 250 (* scale x)))
         (dtry (y)
           (* scale y))
         (try (y)
           (+ 400 (* -1 scale y))))
    (let* ((length cart-pole::*l*)
           (xbase (+ x (* 1/2 length (cos theta))))
           (ybase 0)
           (width 0.5)
           (height 0.1)
           (thickness 2)
           (svg (svg:make-svg-toplevel 'svg:svg-1.1-toplevel :height 500 :width 500))
           (stream (make-string-output-stream)))
      (svg:without-attribute-check
        (svg:text svg (:text-anchor "left"
                       :transform (format nil "translate(~,2f,~,2f)" 0 20))
          (format nil "Episodes: ~:d" cart-pole::*iterations*)))

      (svg:without-attribute-check
        (svg:text svg (:text-anchor "left"
                       :transform (format nil "translate(~,2f,~,2f)" 0 40 ))
          (format nil "Time: ~,1f" time)))
      (svg:draw svg (:rect :x (trx (- xbase (/ width 2))) :y (try ybase) :height (dtry height) :width (dtry width))
                :style "fill:rgb(255,255,255);stroke-width:3;stroke:rgb(0,0,0)")
      (svg:draw svg (:line :x1 (trx xbase) :x2 (trx (+ xbase (* 1/2 length (cos theta))))
                                 :y1 (try ybase) :y2 (try (+ ybase (* 1/2 length (sin theta)))))
                :stroke "black" :stroke-width thickness)
      (svg:draw svg (:line :x1 (trx (- -1 (/ 1/2 length (cos (/ pi 3))))) :x2 (trx (+ 1 (/ 1/2 length (cos (/ pi 3)))))
                           :y1 (try (- height)) :y2 (try (- height)))
                :stroke "black" :stroke-width thickness)
      (svg:stream-out stream svg)
      (get-output-stream-string stream))))

(defun run-ui ()
  (clutter/html:close-app)
  (clutter/html:run-app
   'cart-pole-app)
  (loop for time = 0 do
    (cart-pole::play cart-pole::*initial-state*
                     cart-pole::*δt*
                     (lambda (x theta)
                       (incf time cart-pole::*δt*)
                       (emit (list x theta time) *state*)
                       (sleep (* 0.5 cart-pole::*δt*))))))
