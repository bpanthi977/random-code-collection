(ql:quickload :einsum)
(defpackage :tensor
  (:use :cl #:einsum)
  (:export
   #:initialize-network
   #:forward
   #:update-weights))

(in-package :tensor)

;;;; Data Types

;;; Tensor
(defstruct (tensor (:constructor make-tensor%))
  storage
  gradient
  gradient-batch
  (produced-by nil :type (or null operation)))

(defun tensor-dimesions (tensor)
  (array-dimensions (tensor-storage tensor)))

(export 'storage)
(defun storage (tensor)
  (tensor-storage tensor))

(export 'gradient)
(defun gradient (tensor)
  (tensor-gradient tensor))

(export 'gradient-batch)
(defun gradient-batch (tensor)
  (tensor-gradient-batch tensor))

(export 'make-tensor)
(defun make-tensor (&rest dimensions)
  (make-tensor% :storage (make-array dimensions)
                :gradient (make-array dimensions)
                :gradient-batch (make-array dimensions)))

(export 'to-tensor)
(defun to-tensor (array)
  (make-tensor% :storage array
                :gradient (make-array (array-dimensions array))
                :gradient-batch (make-array (array-dimensions array))))

(defmethod vector-size ((tensor tensor))
  (with-slots (storage) tensor
    (assert (= (array-rank storage)))
    (array-dimension storage 0)))

(defun add-gradient-to-batch (tensor)
  (let ((size (reduce #'* (tensor-dimesions tensor)))
        (g (gradient tensor))
        (gb (gradient-batch tensor)))
    (loop for i from 0 below size do
          (incf (row-major-aref gb i) (row-major-aref g i)))))

(defun set-gradient-zero (tensor)
  (let ((size (reduce #'* (tensor-dimesions tensor)))
        (g (gradient tensor)))
    (loop for i from 0 below size do
      (setf (row-major-aref g i) 0))))

;;; Operation on Tensors
(defclass operation ()
  ((result :type tensor)
   (path-count :type fixnum :accessor path-count)))

(defmethod initialize-instance :around ((o operation) &key)
  (call-next-method)
  (setf (tensor-produced-by (slot-value o 'result)) o))

(defgeneric forward (operation))
(defgeneric compute-gradient (operation))
(defgeneric inputs (operation))

(export 'recompute)
(defmethod recompute ((tensor tensor))
  (with-slots (produced-by) tensor
    (when produced-by
      (mapcar #'recompute (inputs produced-by))
      (forward produced-by))
    (storage tensor)))


(export 'backprop-gradients)
(defmethod backprop-gradients ((tensor tensor) &optional weights)
  (with-slots (gradient) tensor
    (let ((size (array-total-size gradient)))
      (loop for i from 0 below size do
            (setf (row-major-aref gradient i)
                  (if weights
                      (row-major-aref weights i)
                      1)))
      (labels ((rec (tensor f)
                 (with-slots (produced-by) tensor
                   (when produced-by
                     (funcall f produced-by)
                     (map nil (lambda (tensor)
                                (rec tensor f))
                          (inputs produced-by))))))
        (rec tensor (lambda (o)
                      (setf (path-count o) 0)))
        (rec tensor (lambda (o)
                      (incf (path-count o))
                      (when (= 1 (path-count o))
                        (map 'nil #'set-gradient-zero (inputs o)))))
        (rec tensor (lambda (o)
                      (decf (path-count o))
                      (when (= 0 (path-count o))
                        (compute-gradient o)
                        (map 'nil #'add-gradient-to-batch (inputs o)))))
        gradient))))

(export 'parameters)
(defmethod parameters ((tensor tensor))
  (let ((list nil))
    (labels ((rec (tensor)
               (with-slots (produced-by) tensor
                 (if produced-by
                     (mapcar #'rec (inputs produced-by))
                     (push tensor list)))))
      (rec tensor)
      list)))

;;; Sum Operation
(defclass sum (operation)
  ((inputs :initarg :inputs :accessor inputs)))

(defmethod initialize-instance :after ((o sum) &key)
  (with-slots (inputs) o
    (assert (> (length inputs) 0))
    (reduce (lambda (t1 t2)
              (assert (equal (tensor-dimesions t1)
                             (tensor-dimesions t2)))
              t1)
            inputs)
    (unless (slot-boundp o 'result)
      (setf (slot-value o 'result)
            (apply #'make-tensor (tensor-dimesions (first inputs)))))))

(export '->sum)
(defun ->sum (&rest tensors)
  (slot-value (make-instance 'sum :inputs tensors)
              'result))

(defmethod forward ((o sum))
  (with-slots (inputs result) o
    (let ((size (reduce #'* (tensor-dimesions result))))
      (loop for i from 0 below size do
        (setf (row-major-aref (storage result) i)
              (loop for input in inputs
                    summing (row-major-aref (storage input) i)))))))

(defmethod compute-gradient ((o sum))
  (with-slots (inputs result) o
      (let ((dL/dresult (gradient result))
            (size (reduce #'* (tensor-dimesions result))))
        (loop for input in inputs
              for dL/dinput = (gradient input) do
              (loop for i from 0 below size do
                    (incf (row-major-aref dL/dinput i)
                          (row-major-aref dL/dresult i)))))))

;;;; Product
(defclass product (operation)
  ((inputs :initarg :inputs :accessor inputs)))

(defmethod initialize-instance :after ((o product) &key)
  (with-slots (inputs) o
    (assert (> (length inputs) 0))
    (reduce (lambda (t1 t2)
              (cond ((tensor-p t2)
                     (assert (equal (tensor-dimesions t1)
                                    (tensor-dimesions t2))))
                    (t (numberp t2))))
            inputs)
    (unless (slot-boundp o 'result)
      (setf (slot-value o 'result)
            (apply #'make-tensor (tensor-dimesions (first inputs)))))))

(export '->product)
(defun ->product (&rest tensors)
  (slot-value (make-instance 'product :inputs tensors)
              'result))

(defmethod inputs ((o product))
  (remove-if-not #'tensor-p (slot-value o 'inputs)))

(defmethod forward ((o product))
  (with-slots (inputs result) o
    (let ((size (array-total-size (storage result))))
      (loop for i from 0 below size do
        (setf (row-major-aref (storage result) i)
              (loop for input in inputs
                    with product = 1.0d0 do
                      (setf product (* product (if (numberp input)
                                                   input
                                                   (row-major-aref (storage input) i))))
                    finally (return product)))))))

(defmethod compute-gradient ((o product))
  (with-slots (inputs result) o
      (let ((dL/dresult (gradient result))
            (size (reduce #'* (tensor-dimesions result)))
            (tensor-inputs (remove-if-not #'tensor-p inputs)))
        (loop for input in tensor-inputs
              for rest = (remove input inputs)
              for dL/dinput = (gradient input) do
              (loop for i from 0 below size do
                    (incf (row-major-aref dL/dinput i)
                          (* (row-major-aref dL/dresult i)
                             (loop for inp in rest
                                   with product = 1.0d0 do
                                   (setf product (* product (if (numberp inp)
                                                                inp
                                                                (row-major-aref (storage inp) i))))
                                   finally (return product)))))))))

;;; Linear Connections
(defclass linear (operation)
  ((weights :type tensor)
   (bias :type tensor)
   (input :type tensor :initarg :input)
   (result-size :type fixnum :initarg :result-size)))

(defmethod initialize-instance :after ((o linear) &key result-size)
  (unless (slot-boundp o 'weights)
    (setf (slot-value o 'weights)
          (make-tensor result-size (vector-size (slot-value o 'input)))))
  (unless (slot-boundp o 'bias)
    (setf (slot-value o 'bias)
          (make-tensor result-size)))
  (unless (slot-boundp o 'result)
    (setf (slot-value o 'result)
          (make-tensor result-size))))

(export '->linear)
(defun ->linear (input result-size)
  (slot-value (make-instance 'linear :result-size result-size :input input)
              'result))

(defmethod inputs ((o linear))
  (list (slot-value o 'weights)
        (slot-value o 'bias)
        (slot-value o 'input)))

(defmethod forward ((o linear))
  (let ((weights (storage (slot-value o 'weights)))
        (bias (storage (slot-value o 'bias)))
        (input (storage (slot-value o 'input)))
        (result (storage (slot-value o 'result))))
    (einsum (ij :to i) :into result
            (* (j input) (ij weights)))
    (einsum (i :to i) :into result
            (+ (i result) (i bias)))))

(defmethod compute-gradient ((o linear))
  (let ((input (storage (slot-value o 'input)))
        (dL/dinput (gradient (slot-value o 'input)))
        (weights (storage (slot-value o 'weights)))
        (dL/dresult (gradient (slot-value o 'result)))
        (dL/dweights (gradient (slot-value o 'weights)))
        (dL/dbias (gradient (slot-value o 'bias))))
    (einsum (ij :to ij) :into dL/dweights
            (+ (ij dl/dweights)
               (* (j input) (i dL/dresult))))
    (einsum (i :to i) :into dL/dbias
            (+ (i dl/dbias) (i dL/dresult)))
    (einsum (ij :to j) :into dl/dinput
            (+ (j dl/dinput) (* (ij weights) (i dL/dresult))))))

;;;; Activation Functions
(defclass activation (operation)
  ((input :initarg :input)))

(defmethod initialize-instance :after ((o activation) &key)
  (unless (slot-boundp o 'result)
    (setf (slot-value o 'result)
          (apply #'make-tensor (tensor-dimesions (slot-value o 'input))))))

(defmethod inputs ((o activation))
  (list (slot-value o 'input)))

(defclass activation1 (operation)
  ((input :initarg :input)))

(defmethod initialize-instance :after ((o activation1) &key)
  (unless (slot-boundp o 'result)
    (setf (slot-value o 'result)
          (make-tensor (vector-size (slot-value o 'input))))))

(defmethod inputs ((o activation1))
  (list (slot-value o 'input)))

(defclass activation1->0 (operation)
  ((input :initarg :input)))

(defmethod initialize-instance :after ((o activation1->0) &key)
  (unless (slot-boundp o 'result)
    (setf (slot-value o 'result)
          (make-tensor 1))))

(defmethod inputs ((o activation1->0))
  (list (slot-value o 'input)))

;;; Sigmoid
(defclass sigmoid (activation)
  ())

(export '->sigmoid)
(defun ->sigmoid (input)
  (slot-value (make-instance 'sigmoid :input input)
              'result))

(defun sigmoid (x)
  (/ (1+ (exp (- x)))))

(defmethod forward ((o sigmoid))
  (let* ((input (storage (slot-value o 'input)))
         (result (storage (slot-value o 'result)))
         (size (array-total-size result)))
    (loop for i from 0 below size do
      (setf (row-major-aref result i)
            (sigmoid (row-major-aref input i))))))

(defmethod compute-gradient ((o sigmoid))
  (let* ((dL/dinput (gradient (slot-value o 'input)))
         (result (storage (slot-value o 'result)))
         (size (array-total-size result)))
    (loop for i from 0 below size
          for r = (row-major-aref result i) do
      (incf (row-major-aref dl/dinput i)
            (* r (- 1 r))))))

;; ReLU
(defclass ReLU (activation)
  ())

(export '->ReLU)
(defun ->ReLU (input)
  (slot-value (make-instance 'ReLU :input input)
              'result))

(defmethod forward ((o ReLU))
  (let* ((input (storage (slot-value o 'input)))
         (result (storage (slot-value o 'result)))
         (size (array-total-size result)))
    (loop for i from 0 below size
          for x = (row-major-aref input i) do
      (setf (row-major-aref result i)
            (if (> x 0)
                x
                0)))))

(defmethod compute-gradient ((o ReLU))
  (let* ((dL/dinput (gradient (slot-value o 'input)))
         (dL/dresult (gradient (slot-value o 'result)))
         (input (storage (slot-value o 'result)))
         (size (array-total-size input)))
    (loop for i from 0 below size
          for x = (row-major-aref input i) do
      (incf (row-major-aref dl/dinput i)
            (if (> x 0)
                (row-major-aref dl/dresult i) 0)))))

;;; Leaky ReLU
(defclass ReLU2 (activation)
  ((leak-slope :initarg :leak-slope)))

(export '->ReLU2)
(defun ->ReLU2 (input leak-slope)
  (slot-value (make-instance 'ReLU2 :input input :leak-slope leak-slope)
              'result))

(defmethod forward ((o ReLU2))
  (let* ((input (storage (slot-value o 'input)))
         (leak-slope (slot-value o 'leak-slope))
         (result (storage (slot-value o 'result)))
         (size (array-total-size result)))
    (loop for i from 0 below size
          for x = (row-major-aref input i) do
      (setf (row-major-aref result i)
            (if (> x 0)
                x
                (* leak-slope x))))))

(defmethod compute-gradient ((o ReLU2))
  (let* ((dL/dinput (gradient (slot-value o 'input)))
         (input (storage (slot-value o 'result)))
         (dL/dresult (gradient (slot-value o 'result)))
         (leak-slope (slot-value o 'leak-slope))
         (size (array-total-size input)))
    (loop for i from 0 below size
          for x = (row-major-aref input i) do
            (incf (row-major-aref dl/dinput i)
                  (* (row-major-aref dl/dresult i)
                     (if (> x 0)
                         1 leak-slope))))))
;; Softmax
(defclass softmax (activation1)
  ())

(export '->softmax)
(defun ->softmax (input)
  (slot-value (make-instance 'softmax :input input)
              'result))

(defmethod forward ((o softmax))
  (let* ((input (storage (slot-value o 'input)))
         (max (reduce #'max input))
         (result (storage (slot-value o 'result))))

    (einsum (i :to i) :into result
            (exp (- (i input) max)))
    (let ((sum (reduce #'+ result)))
      (einsum (i :to i) :into result
              (/ (i result) sum)))))

(defmethod compute-gradient ((o softmax))
  (let* ((dL/dinput (gradient (slot-value o 'input)))
         (result (storage (slot-value o 'result)))
         (dL/dresult (gradient (slot-value o 'result))))
    (einsum (ij :to j) :into dL/dinput
            (+ (j dL/dinput)
               (* (i dl/dresult)
                  (i result) (- (if (= i j) 1 0) (j result)))))))

;;; Ln(Softmax)
(defclass log-softmax (activation1)
  ())

(export '->log-softmax)
(defun ->log-softmax (input)
  (slot-value (make-instance 'log-softmax :input input)
              'result))

#+nil(defun ->log-softmax (input)
  (->natural-log (->softmax input)))

(defmethod forward ((o log-softmax))
  (let* ((input (storage (slot-value o 'input)))
         (max (reduce #'max input))
         (result (storage (slot-value o 'result))))

    (einsum (i :to i) :into result
            (exp (- (i input) max)))
    (let* ((sum (reduce #'+ result))
           (logsum (log sum)))
      (einsum (i :to i) :into result
              (- (i input) max logsum)))))

(defmethod compute-gradient ((o log-softmax))
  (let* ((dL/dinput (gradient (slot-value o 'input)))
         (result (storage (slot-value o 'result)))
         (dL/dresult (gradient (slot-value o 'result))))
    (einsum (ij :to j) :into dL/dinput
            (+ (j dL/dinput)
               (* (i dL/dresult) (- (if (= i j) 1 0)
                                    (exp (j result))))))))

;;; Ln
(defclass natural-log (activation)
  ())

(export '->natural-log)
(defun ->natural-log (input)
  (slot-value (make-instance 'natural-log :input input)
              'result))

(defmethod forward ((o natural-log))
  (let ((input (storage (slot-value o 'input)))
        (result (storage (slot-value o 'result))))
    (einsum (i :to i) :into result
            (log (i input)))))

(defmethod compute-gradient ((o natural-log))
  (let* ((dL/dinput (gradient (slot-value o 'input)))
         (input (storage (slot-value o 'input)))
         (dL/dresult (gradient (slot-value o 'result))))
    (einsum (i :to i) :into dL/dinput
            (+ (i dl/dinput) (* (i dL/dresult) (/ (i input)))))))

;; Sum all nodes to return a rank 0 tensor
(defclass sum0 (activation1->0)
  ())

(export '->sum0)
(defun ->sum0 (input)
  (slot-value (make-instance 'sum0 :input input)
              'result))

(defmethod forward ((o sum0))
  (let ((input (storage (slot-value o 'input)))
        (result (storage (slot-value o 'result))))
    (setf (aref result 0) (reduce #'+ input))))

(defmethod compute-gradient ((o sum0))
  (let* ((dL/dinput (gradient (slot-value o 'input)))
         (dL/dresult (aref (gradient (slot-value o 'result))
                           0)))
    (einsum (i :to i) :into dl/dinput
            (+ (i dl/dinput) dl/dresult))))

;;; Sum of square of all the nodes
;;; returns a rank 0 tensor
(defclass sum-squared (activation1->0)
  ())

(export '->sum-squared)
(defun ->sum-squared (input)
  (slot-value (make-instance 'sum-squared :input input)
              'result))

(defmethod forward ((o sum-squared))
  (let ((input (storage (slot-value o 'input)))
        (result (storage (slot-value o 'result))))
    (setf (aref result 0) (reduce #'+ input :key (lambda (x) (* x x))))))

(defmethod compute-gradient ((o sum-squared))
  (let* ((dL/dinput (gradient (slot-value o 'input)))
         (input (storage (slot-value o 'input)))
         (dL/dresult (aref (gradient (slot-value o 'result)) 0)))
    (einsum (i :to i) :into dL/dinput (+ (i dl/dinput)
                                         (* dL/dresult (i input))))))
