(defparameter *sentences* (uiop:read-file-form #p"/tmp/sentences"))

;; index-to-word & word-to-index
(defparameter *word-count* (make-hash-table :test #'equal))

(loop for s in *sentences* do 
  (loop for w in s 
	for word = (string-downcase w) 
	for count = (gethash word *word-count* 0) do 
	  (setf (gethash word *word-count*) (1+ count))))

(defun hashtable-keys (hashtable)
  (let ((keys (make-array (hash-table-count hashtable) :fill-pointer 0)))
    (maphash (lambda (key value) 
	       (declare (ignore value))
	       (vector-push key keys))
	     hashtable)
    keys))

(defparameter *sorted-words* (sort (hashtable-keys *word-count*)
				   #'> :key (lambda (word)
					      (gethash word *word-count* 0))))

(defparameter *vocabulary_size* 1000)
(defparameter *index-to-word* (make-array *vocabulary_size*
					  :element-type 'string
					  :initial-element ""
					  :adjustable t 
					  :fill-pointer 3))

(defparameter *word-to-index* (make-hash-table :test #'equal :size *vocabulary_size*))

;; start with index 3. 
(defconstant +SENTENCE-START+ 0)
(defconstant +SENTENCE-END+ 1)
(defconstant +UNKNOWN-WORD+ 2)

(defparameter *data* (make-array (length *sentences*)
				 :fill-pointer 0))

(let ((top-words (make-hash-table :test #'equal))
      (index-counter 2))
  (loop for w across *sorted-words* 
	repeat (- *vocabulary_size* 3) do 
	  (setf (gethash w top-words) t))

  (loop for s in *sentences* 
	for x = (make-array (+ (length s) 2) :fill-pointer 0) do 
	  (vector-push +sentence-start+ x)
	  (loop for w in s 
		for word = (string-downcase w)
		for index = (gethash word *word-to-index*) do 

		  (if (gethash word top-words) 
		      (progn 
			(unless index 
			  (setf index (incf index-counter))
			  (setf (gethash word *word-to-index*) index)
			  (vector-push word *index-to-word*))
			(vector-push index x))
		      (vector-push +unknown-word+ x)))
	  (vector-push +sentence-end+ x)
	  (setf (fill-pointer x) (+ (length s) 1))

	  (vector-push-extend 
	   (list x 
		 (make-array (+ (length s) 1)
			     :displaced-to x 
			     :displaced-index-offset 1))
	    *data*)))

(defun index-word (index)
  "Get word from index"
  (aref *index-to-word* index))

(defun word-index (word)
  "Get index from word"
  (gethash word *word-to-index*))

(defun make-random-matrix (m n)
  (let ((matrix (make-array (list m n) :element-type 'double-float 
			    :initial-element 0d0))
	(1/sqrtn (coerce (/ (sqrt n)) 'double-float)))
    (loop for i from 0 below m do 
      (loop for j from 0 below n 
	    for random = (random (* 2 1/sqrtn)) do 
	    (setf (aref matrix i j) 
		  (- random 1/sqrtn))))
    matrix))

(defclass network () 
  ((H :accessor H :initarg :H)
   (C :accessor C :initarg :C)
   (U :accessor U )
   (V :accessor V ) 
   (W :accessor W )))

(defmethod initialize-instance :after ((n network) &key)
  (with-slots (H C) n 
    (setf (slot-value n 'U) (make-random-matrix H C))
    (setf (slot-value n 'V) (make-random-matrix C H))
    (setf (slot-value n 'W) (make-random-matrix H H))))

(defun matrix-dot-vector (matrix vector)
  "pointwise operate f on matrix . vector"
  (let* ((m (array-dimension matrix 0))
	 (n (array-dimension matrix 1))
	 (result (make-array m :element-type 'double-float :initial-element 0d0
			       :fill-pointer 0)))
    (loop for i from 0 below m do 
      (vector-push (loop for j from 0 below n 
			 summing (* (aref matrix i j)
				    (aref vector j)))
		   result))
    result))

(defun matrix-dot-index (matrix index)
  "matrix . vector; where vector is one shot representation of index"
  (let* ((m (array-dimension matrix 0))
	 (n (array-dimension matrix 1))
	 (result (make-array m :element-type 'double-float :initial-element 0d0
			       :fill-pointer 0)))
    (assert (< index n))
    (loop for i from 0 below m do 
      (vector-push (aref matrix i index)
		   result))
    result))

(defun softmax% (vector)
  "Destructively calculates softmax"
  (map-into vector (lambda (x) (exp x)) vector)
  (let ((sum (reduce #'+ vector)))
    (map-into vector (lambda (x) (/ x sum)) vector)
    vector))

(defun map-into2 (function &rest sequences)
  "map `function' into the first of the `sequences'"
  (apply #'map-into (first sequences) function sequences))

(defmethod forward-propagate ((n network) x)
  (with-slots (U V W) n
    (let ((s (make-array (length x))) ;; hidden state at each timestep
	  (p (make-array (length x)))) ;; output at each timestep
      (loop for time from 0 below (length x) 
	    for xt = (aref x time) do 
	(setf (aref s time) 
	      (if (= time 0)
		  (matrix-dot-index U xt)
		  (map-into2 (lambda (x y) 
			       (tanh (+ x y)))
			     (matrix-dot-index U xt)
			     (matrix-dot-vector W (aref s (1- time))))))
	(setf (aref p time)
	      (softmax% (matrix-dot-vector V (aref s time)))))
      (values p s))))

(defun loss (output y-indices)
  "Loss for a single sentence; 
output `output' from the network, the actual target `y' "
  (/ (loop for p_t across output
	   for y across y-indices
	   summing (log (aref p_t y)))
     -1))

(defmethod calculate-total-loss ((n network) inputs targets)
  "Loss for all sentences `inputs' and `outputs'"
  (/ (loop for input in inputs 
	   for target in targets 
	   summing (loss (forward-propagate n input) target))
     (loop for i in inputs summing (length i))))

(defun incf-outer-product (place vec-a vec-b)
  "Add the outer product of `vec-a' and `vec-b' into `place'"
  (let ((n (array-dimension place 0))
	(m (array-dimension place 1)))
    (assert (= n (length vec-a)))
    (assert (= m (length vec-b)))
    (loop for i from 0 below n do 
      (loop for j from 0 below m do 
	(incf (aref place i j) 
	      (* (aref vec-a i)
		 (aref vec-b j)))))
    place))

(defun incf-outer-product-with-index (place vec-a vec-b-index)
  (let ((n (array-dimension place 0))
	(m (array-dimension place 1)))
    (assert (= n (length vec-a)))
    (assert (< vec-b-index m))
    (loop for i from 0 below n do 
      (incf (aref place i vec-b-index)
	    (aref vec-a i)))
    place))

(defun transpose (matrix)
  (destructuring-bind (m n) (array-dimensions matrix)
    (let ((new-matrix (make-array (list n m))))
      (loop for i from 0 below m do
	(loop for j from 0 below n do
	  (setf (aref new-matrix j i)
		(aref matrix i j))))
      new-matrix)))

(defun matrix-T-dot-vector (matrix vector)
  "Multiply transpose of `matrix' with `vector'"
  (destructuring-bind (m n) (array-dimensions matrix)
    (assert (= m (length vector)))
    (let ((result (make-array n)))
      (loop for j from 0 below n do 
	(setf (aref result j) 
	      (loop for i from 0 below m 
		    summing (* (aref matrix i j)
			       (aref vector i)))))
      result)))

(define-compiler-macro matrix-dot-vector (&whole expr matrix vector)
  (cond
    ((and (listp matrix)
	  (eql (first matrix) 'transpose)
	  (eql (length matrix) 2))
     `(matrix-T-dot-vector ,(second matrix) ,vector))
    (t expr)))


(declaim (inline modify-indexed))
(defun modify-indexed (f a index)
  (setf (aref a index) (funcall f (aref a index) 1))
  a)

(defun modify (f a b)
  "Alternative of map-into, that works when `b' is index vectors too. 
Modify vector `a_i' with' f(a_i,b_i)"
  (if (integerp b)
      (modify-indexed f a b)
      (map-into a f a b)))

(define-compiler-macro modify (&whole expr f a b &environment env)
  "optimize `modify' for indexed vector b"
  (if (constantp b env)
      (typecase b 
	(integer `(modify-indexed ,f ,a ,b))
	(array expr)
	(t "error"))
      (cond ((atom b) expr)
	    ((or (eql (first b) 'deindex)
		 (and (eql (first b) 'the) 
		      (eql (second b) 'integer)))
	     ;; b is an index variable 
	     `(modify-indexed ,f ,a ,b))
	    (t expr))))


(defun deindex (y)
  "Return normal vector from the index vector `y'; assume size = *vocabulary-size*"
  (warn "Don't use this function directly. This function must be 
intercepted by compiler macros and converted to efficient code")
  (let ((result (make-array *vocabulary_size*)))
    (setf (aref result y) 1)
    result))


(defun bptt (n x y &key (bptt-truncate 4))
  (declare (optimize (debug 3)))
  (check-type n network)
  (with-slots (U V W) n 
    (multiple-value-bind (p s) (forward-propagate n x)
      ;; p[t] = softmax(o[t] = V s[t]), s[t] = tanh(Ux + W s[t-1])
      (let ((dL/dU (make-array (array-dimensions (U n))))
	    (dL/dV (make-array (array-dimensions (V n))))
	    (dL/dW (make-array (array-dimensions (W n))))
	    dL/do
	    (di (make-array (array-dimension W 0))))

	;; replace p with dL/do = p - y 
	(map-into p (lambda (p[t] y[t])
		      (modify #'- p[t] (the integer y[t])))
		  p y)
	(setf dL/do p)

	;; for each output backwards
	(loop for time from (1- (length y)) downto 0 do 
	  ;; dL/dV += L_i s[t]^j
	  (incf-outer-product dL/dV
			      (aref dL/do time) (aref s time))

	  ;; di = L_k V^k_i [ 1 - (s^i)^2]
	  (loop for i from 0 below (length di) do
	    (setf (aref di i)
		  (* (- 1 (expt (aref (aref s time) i) 2))
		     (loop for k from 0 below (array-dimension V 1)
			   summing (* (aref (aref dL/do time) k)
				      (aref V k i))))))

	  ;; accumulate error for bptt-truncate steps back in time
	  (loop for time2 from time downto (max 0 (- time bptt-truncate)) do 
	    ;; dL/dW += d_i s^j_,-1 + d_i,-1 s^j_,-2 + ... 
	    (unless (= 0 time2)
	      (incf-outer-product dL/dW 
				  di (aref s (- time2 1))))
	    ;; dL/dU += d_i x^j + d_i,-1 x^j_,-1 + ... 
	    (incf-outer-product-with-index dL/dU 
					   di (aref x time))

	    ;; d_i,-n = d_m,-n+1 W^m_i [ (1 - (s^i_, -n)^2)]
	    (unless (= 0 time2)
	      (map-into di 
			(lambda (dW_i s^i)
			  (* dW_i (- 1 (expt s^i 2))))
			(matrix-t-dot-vector W di)
			(aref s time2)))))
	(values dL/dU dL/dV dL/dW)))))

(defun update-matrix (M diff rate)
  (loop for i from 0 below (array-dimension M 0) do 
    (loop for j from 0 below (array-dimension M 1) do 
	  (setf (aref M i j)
		(- (aref M i j)
		   (* rate (aref diff i j)))))))

(defmethod sgd-step ((n network) x y learning-rate)
  (multiple-value-bind (dL/dU dL/dV dL/dW) (bptt n x y)
    (with-slots (U V W) n 
      (update-matrix U dL/dU learning-rate)
      (update-matrix V dL/dV learning-rate)
      (update-matrix W dL/dW learning-rate))))

(defmethod train-with-sgd ((n network) xs ys 
			   &key (learning-rate 0.005) (epoch 100)
			     (evaluate-loss-after 5))
  (loop for i from 0 below epoch 
	with losses = nil 
	for clock-time = (get-internal-real-time) do 
    (when (and (not (= i 0))
	       (= 0 (mod i evaluate-loss-after)))
      (push (calculate-total-loss n xs ys) losses)
      (format t "~& Loss = ~f" (first losses))
      (when (and (> (length losses) 1)
		 (> (first losses) (second losses)))
	;; when loss increased 
	(setf learning-rate (* 0.5 learning-rate))
	(format t "~&Loss increased; so learning-rate is decreased to ~f" learning-rate)))

    (loop for y in ys 
	  for x in xs 
	  for i from 0 do 
	  (sgd-step n x y learning-rate)
	  (when (= 0 (mod i 50))
	    (format t "~&     ~d examples learned" i)))

    (format t "~& Epoch ~d done in ~f seconds." i
	    (/ (- (get-internal-real-time) clock-time)
	       internal-time-units-per-second))))
