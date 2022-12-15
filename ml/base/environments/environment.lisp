(in-package #:ml/environment)

;;; State and Integrator
(defmacro defstate (name &rest components)
  `(progn
     (defun ,(intern (format nil "MAKE-~a-STATE"
                         (symbol-name name)))
         (&key ,@(loop for c in components
                       collect (list c 0.0d0)))
       (vector ,@components))

     ,@(loop for c in components
             for i from 0
             collect `(defun ,(intern
                           (format nil "~a-~a"
                                   (symbol-name name)
                                   (symbol-name c)))
                          (state)
                        (elt state ,i)))))

(defun state+  (state1 state2)
  (map 'vector #'+ state1 state2))

(defun state* (k state)
  (map 'vector (lambda (component)
                 (* k component))
       state))

(defun euler-forward (state dS/dt Δt)
  (state+ (state* Δt (funcall dS/dt state))
          state))

(defun rk4 (state dS/dt Δt)
  (let* ((k₁ (funcall dS/dt state))
         (k₂ (funcall dS/dt (state+ state (state* (/ Δt 2) k₁))))
         (k₃ (funcall dS/dt (state+ state (state* (/ Δt 2) k₂))))
         (k₄ (funcall dS/dt (state+ state (state* Δt k₃))))
         (k (state* (/ Δt 6) (reduce #'state+
                                     (list k₁
                                           (state* 2 k₂)
                                           (state* 2 k₃)
                                           k₄)))))
    (state+ state k)))

;;; Environment
;; An environment has to implement the following three methods
(defgeneric initialize (environment)
  (:documentation "Return the initial state of environment"))

(defgeneric terminated? (environment state)
  (:documentation "Returns a boolean indicating if the state has terminated or not"))

(defgeneric take-action (environment state action)
  (:documentation "Take the `action' at current `state' in the `environment' and return new state"))

;;; Samplers
(defun max-reward (alpha steps)
  (loop repeat steps
        for r = 1 then (* alpha r)
        summing r))

(defun sampler/n-steps-return (policy environment n &key discount-rate steps-limit)
  ;; TODO take reward function as input
  (let ((buffer (u:make-ring-buffer n)) ;; state action reward buffer
        state terminated
        steps-count)
    (flet ((forward-state ()
             (incf steps-count)
             (when (= steps-count steps-limit)
               (setf terminated t))
             (let* ((action (funcall policy state))
                    (new-state (take-action environment state action)))
               (when (terminated? environment new-state)
                 (setf terminated t))
               (u:insert-item (list state action 1) buffer)
               (setf state new-state))))
    (lambda ()
      (when (= 0 (u:size buffer))
        (setf terminated nil)
        (setf state (initialize environment))
        (setf steps-count 0)
        (loop repeat n do
          (forward-state)
          (when terminated
            (return))))
      (let ((sar (u:delete-item buffer))
            (l (+ 1 (u:size buffer))))
        (unless terminated
          (forward-state))
        (values (first sar) (second sar) (max-reward discount-rate l)
                (= l 1)))))))
