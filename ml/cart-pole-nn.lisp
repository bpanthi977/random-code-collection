;;;; cart-pole.lisp
(ql:quickload :ml)
(defpackage #:cart-pole
  (:use #:cl)
  (:local-nicknames (#:u #:ml/utils)
                    (#:t #:ml/tensor)))

(in-package #:cart-pole)

(defconstant +state-size+ 4)
(defconstant +action-size+ 2)
(defconstant +max-steps+ 200)

(defparameter *policy* nil)
(defparameter *environment* :cart-pole)
(defparameter *episodes* 0)

;;; Policy Network
(defclass policy ()
  ((input :initarg :input)
   (probability :initarg :probability)
   (log-probability :initarg :log-probability)
   (parameters :initarg :parameters)))

(defun make-policy-network (&key (n-hidden 128))
  (setf *episodes* 0)
  (let* ((input (t:make-tensor +state-size+))
         (hidden (t:->relu2 (t:->linear input n-hidden :initialization #'t:he) 0.01))
         (action-preference (t:->linear hidden +action-size+ :initialization #'t:xavier))
         (probabilities (t:->softmax action-preference))
         (log-probability (t:->log-softmax action-preference))
         (parameters (remove input (t:parameters action-preference))))
    (make-instance 'policy
                   :input input
                   :probability probabilities
                   :log-probability log-probability
                   :parameters parameters)))

(defmethod set-input ((fnn policy) state)
  (t:set-tensor (slot-value fnn 'input) state))

(defmethod action ((fnn policy) state)
  (set-input fnn state)
  (let* ((probabilities (t:recompute (slot-value fnn 'probability)))
         (prob (random 1.0)))
    (or (loop for p across probabilities
              for i from 0
              summing p into sum
              when (>= sum prob)
                return i)
        0)))

;;; Train
(defparameter *discount-rate* 0.99)
(defparameter *α-policy* 1)

(defun learn% (policy environment)
  (let ((sampler (ml/environment:sampler/n-steps-return
                  (lambda (state)
                    (action policy state))
                  environment
                  10
                  :steps-limit 200
                  :discount-rate *discount-rate*))
        (adam (ml/optimizer:adam (slot-value policy 'parameters)))
        ;; parameters
        (baseline (u:make-moving-average-buffer 50000))
        (batch-steps 32)
        (entropy-beta 0.01)
        ;; logging
        (mean-steps (u:make-moving-average-buffer 100))
        (total-steps 0)
        (episode-steps 0))

    (with-slots (log-probability probability input parameters) policy
      ;; Define Loss function
      (let* ((action-mask-and-factor (t:make-tensor 2))
             (policy-loss
               (t:->sum0
                (t:->product log-probability
                             action-mask-and-factor
                             -1)))
             (entropy-loss
               (t:->sum0
                (t:->product probability log-probability
                             entropy-beta)))
             (loss (t:->product (t:->sum entropy-loss policy-loss)
                                (/ batch-steps))))

        ;; Training episodes
        (loop for i from 1 do
          (multiple-value-bind (state action reward terminated) (funcall sampler)
            (u:ma-add baseline reward)
            (incf total-steps)
            (incf episode-steps)
            ;; compute gradients
            (set-input policy state)
            (let ((factor (- reward (u:ma-average baseline))))
              (t:set-tensor action-mask-and-factor
                            (if (= action 0)
                                (vector factor 0)
                                (vector 0      factor))))
            (t:recompute loss)
            (t:backprop-gradients loss)

            ;; optimize
            (when (= 0 (mod i batch-steps))
              (funcall adam))

            ;; Logging
            (when terminated
              (incf *episodes*)
              (u:ma-add mean-steps episode-steps)
              (when (= (mod *episodes* batch-steps) 0)
                (format t "~&S: ~d, steps: ~d, R_100: ~,2f, Ep: ~d Baseline: ~,2f~%"
                        total-steps episode-steps (u:ma-average mean-steps) *episodes*
                        (u:ma-average baseline)))
              (setf episode-steps 0))))))))

(defun learn ()
  (setf *policy* (make-policy-value-fnn))
  (setf *environment* :cart-pole-classic)
  (learn% *policy* :cart-pole-classic))


;;; Rendering
(defun play (render-function)
  (loop with state = (ml/environment:initialize *environment*)
        for action = (action *policy* state)
        for new-state = (ml/environment:take-action *environment* state action)
        for reward  = 1 then (* reward *discount-rate*)
        for time from 0
        for total-reward = reward then (+ total-reward reward) do
          (when render-function (funcall render-function (elt state 0)
                                         (if (eql *environment* :cart-pole-classic)
                                             (elt state 1)
                                             (- (/ pi 2)
                                                (elt state 1)))))
          (setf state new-state)
          (when (ml/environment:terminated? *environment* new-state)
            (return (values time total-reward)))))
