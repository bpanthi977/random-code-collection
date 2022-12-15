(defpackage :ml/test/adam
  (:use :cl)
  (:local-nicknames (#:t #:ml/tensor)
                    (#:o #:ml/optimizer)))

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

;;; Train & Test

(defun train (net)
  (let ((adam (o:adam (getf net :parameters)
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
