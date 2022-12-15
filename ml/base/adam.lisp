(ql:quickload :serapeum)
;;(load "./tensor.lisp")

(uiop:define-package  :utils
    (:use :serapeum :alexandria)
  (:reexport :serapeum
   :alexandria))

(defpackage :ml/test
  (:use :cl)
  (:local-nicknames (#:t #:tensor)
                    (#:u #:utils)))

(in-package :ml/test)

;; Utility and data setup
(defun set-tensors-elements (tensors fn &optional (key #'t:storage))
  (loop for tensor in tensors
        for s = (funcall key tensor)
        for size = (reduce #'* (array-dimensions s)) do
        (loop for i from 0 below size do
              (setf (row-major-aref s i)
                    (funcall fn)))))

(defmethod set-input (network x y)
  (let ((input (t:storage (getf network :input)))
        (answer (t:storage (getf network :answer))))
    (setf (aref input 0) x)
    (setf (aref answer 0) y)))

(defmacro map-combined-arrays (arrays (i el) &body body)
  (u:with-gensyms (arr k)
  `(loop with ,i = 0
         for ,arr in ,arrays do
           (loop for ,k from 0 below (array-total-size ,arr) do
                 (symbol-macrolet ((,el
                                     (row-major-aref ,arr ,k)))
                   ,@body)
                 (incf ,i)))))

(defun f (x)
  (+ (* 123.232 x)
     (/ (+ 2 x)
        (expt x 3.4))))

(defparameter *data*
  (loop repeat 10000
        for x = (+ 1 (random 1000.d0))
        collect (cons x (f x))))

(defparameter *train* (subseq *data* 0 9000))
(defparameter *test* (subseq *data* 9000 10000))

;;; Network
(defparameter *network* nil)
(defun make-f-network (&optional (n-hidden 32))
  (let* ((input (t:make-tensor 1))
         (answer (t:make-tensor 1))
         (hidden-activation (t:->linear input n-hidden))
         (hidden (t:->relu2 hidden-activation 0.002))
         (output (t:->linear hidden 1))
         (loss (t:->sum-squared (t:->sum output (t:->product answer -1))))
         (parameters (remove input (t:parameters output))))
    (set-tensors-elements parameters (lambda ()
                                       (- (random 2.0d0) 1.0d0)))
    (list :output output
          :loss loss
          :parameters parameters
          :input input
          :answer answer)))

;;; Adam Optimizer
(defun adam-optimizer (parameters &key
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

;;; Train & Test

(defun train (net)
  (let ((adam (adam-optimizer (getf net :parameters)
                              :alpha 0.001)))
    (loop for (x . y) in *train*
          with loss = 0.0d0
          for i from 1 do
            (set-input net x y)
            (t:recompute (getf net :loss))
            (incf loss (aref (t:storage (getf net :loss)) 0))
            (t:backprop-gradients (getf net :loss))
            (when (= 0 (mod i 32))
              (funcall adam))
          finally (return loss))))

(defun test (net &optional (test *test*))
  (loop for (x . y) in test
        with loss = 0.0d0 do
          (set-input net x y)
          (t:recompute (getf net :loss))
          (incf loss (aref (t:storage (getf net :loss)) 0))
        finally (return loss)))

(defun main ()
  (setf *network* (make-f-network))
  (loop for epoch from 1 below 1000 do
    (format t "~&Epoch: ~d,~%Train Loss: ~,3f~%" epoch (train *network*))
    (format t "~&Test Loss: ~,3f~%" (test *network*))))
