(defpackage :backprop
  (:use :cl))

(in-package :backprop)

(defun activation (weights inputs)
  (assert (= (length inputs) (1- (length weights))))
  (loop with activation = (elt weights 0) 
	for x across inputs 
	for i from 1 
	summing (* (aref weights i) 
		   x)))

(defun transfer (activation)
  (/ (1+ (exp (- activation)))))

(defstruct network 
  weights 
  outputs 
  errors)

(defun initialize-network (num-neurons)
  (let ((weights (initialize-network-weights num-neurons)))
    (make-network :weights weights 
		  :outputs (make-array (1- (length num-neurons))
				       :initial-contents 
				       (loop for n in (rest num-neurons) 
					     collect (make-array n :element-type 'double-float)))
		  :errors (make-array (1- (length num-neurons))
				      :initial-contents 
				      (loop for n in (rest num-neurons) 
					    collect (make-array n :element-type 'double-float))))))
(defun output (network)
  "Output of the last layer of the network"
  (let ((outputs (network-outputs network)))
    (aref outputs (- (length outputs) 1))))

(defun forward-propagate (network input)
  (loop for layer-weights across (network-weights network) 
	for layer-outputs across (network-outputs network) do 
	  (map-into layer-outputs 
		    (lambda (weights)
		      (transfer (activation weights input)))
		    layer-weights)
	  (setf input layer-outputs)
	finally (return layer-outputs)))

(defun transfer-derivative (output)
  (* output (- 1 output)))

(defun backpropagate-error (network expected)
  (with-slots (weights outputs errors) network 
    ;; errors at output neurons 
    (let ((err (aref errors (1- (length errors)))))
      (map-into err 
		(lambda (o e)
		  (* (- o e) 
		     (transfer-derivative o)))
		(aref outputs (1- (length outputs)))
		expected))

    ;; error at neurons in hidden layers 
    ;; loop thorugh layers 
    (loop for i from (- (length errors) 2) downto 0 
	  for err_i+1 = (aref errors (1+ i))
	  for err_i = (aref errors i)
	  for output_i = (aref outputs i) 
	  for weights_i = (aref weights i) do 
	    ;; loop thorugh each neuron in the layer
	    (loop for o across output_i 
		  for j from 0 do 
		    ;; set error 
		    (setf (aref err_i j)
			  (* (transfer-derivative o)
			     (loop for err across err_i+1 
				   for k from 0 
				   summing (* (aref (aref weights_i k) j)
					      err))))))))

(defun update-weights (network input learning-rate)
  ;; loop across layer
  (loop for weights across (network-weights network) 
	for output across (network-outputs network)
	for err across (network-errors network) do 
	  ;; loop across neurons
	  (loop for e across err 
		for i from 0 
		for neuron-weights across weights do 
		  (loop for w across neuron-weights 
			for k from 0 do 
			  (setf (aref neuron-weights k) 
				(- w (* e learning-rate 
					(if (= k 0) 1 (aref input (1- k))))))))

	  ;; input for next layer is output of current layer 
	  (setf input output)))

(defun train-network (network data learning-rate epochs)
  (loop for epoch from 1 to epochs
	for total-error = 0d0 do 
	  (loop for (input expected-output) in data do 
	    (forward-propagate network input)
	    ;; calculate error 
	    (incf total-error 
		  (loop for output across (output network)
			for expected across expected-output 
			summing (* 1/2 (expt (- output expected) 2))))
	    (backpropagate-error network expected-output)
	    (update-weights network input learning-rate))
	  (format t "~&epoch=~d, ~tlearning-rate=~,3f ~terror=~,3f"
		  epoch learning-rate total-error)))

(defun argmax (vector)
  (loop with h = (aref vector 0) 
	with hi = 0 
	for i from 1 below (length vector)
	for v = (aref vector i) do 
	  (when (> v h)
	    (setf h v
		  hi i))
	finally (return hi)))

(defun predict (network input)
  (forward-propagate network input)
  (argmax (output network)))
