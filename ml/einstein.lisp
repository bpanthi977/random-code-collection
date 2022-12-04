;; see https://github.com/bpanthi977/einsum for current developments. And asdf loadable library

(defpackage :einsum
  (:use :cl)
  (:export #:einsum))

(in-package :einsum)

(defparameter *indices* nil)
(defparameter *indices-vars* nil)
(defparameter *indices-max* nil)
(defparameter *constant-indices* nil)

(defun index-max (index)
  "Returns gensymed variable used to denote dimension of `index'"
  (nth (position index *indices*) *indices-max*))

(defun index-var (index)
  "Returns gensymed variable used as looping variable of `index'"
  (nth (position index *indices*) *indices-vars*))

(defun index-function? (symbol)
  "Checks if the `symbol' could be name of a indexing function"
  (loop for char across (symbol-name symbol) do 
	(unless (find char *indices*)
	  (return nil))
	finally (return t)))

(defun walk-for-dimensions (expr &optional results)
  "Look at `expr' and find out the which index corresponds to which dimension of which tensor
returns list of (index tensor axis)"
  (cond ((or (atom expr) (not *indices*)) nil)
	((and (listp expr)
	      (index-function? (first expr))
	      (= (length expr) 2)
	      (symbolp (second expr)))
	 (loop for char across (symbol-name (first expr)) 
	       for i from 0 do
		 (pushnew (list char (second expr) i)
			  results :test #'equal)))
	(t (loop for subexpr in expr do 
		 (setf results (walk-for-dimensions subexpr results)))))
  results)

(defun expand-arefs (expr)
  "Repalce indexing functions with aref in the expression `expr'"
  (cond ((atom expr)
	 expr)
	((and (listp expr)
	      (index-function? (first expr))
	      (= (length expr) 2))
	 `(aref ,(second expr) ,@(loop for index across (symbol-name (first expr)) 
				       collect (index-var index))))
	((listp expr)
	 (mapcar (lambda (e)
		   (expand-arefs e))
		 expr))
	(t expr)))

(defun constant-expr? (e)
  "Returns true if expression `e' is constant when indices in `*constant-indices*' are given"
  (cond ((atom e) t)
	((and (listp e)
	      (index-function? (first e)))
	 (loop for i across (symbol-name (first e)) do 
	   (if (not (find i *constant-indices*))
	       (return nil))
	       finally (return t)))
	((listp e)
	 (every (lambda (e) 
		  (constant-expr? e))
		(rest e)))
	(t t)))
	 
(defun extract-constant-expr (expr)
  "Return constant and non-constant parts in `expr' under given `*constant-indices*'"
  (let* ((non-constant-expr (remove-if (lambda (e)
					 (constant-expr? e))
				       expr))
	 (constant-expr (set-difference expr non-constant-expr)))
    (values constant-expr non-constant-expr)))

(defun dimension-assertions (dimensions)
  "return assert forms; `dimesions' is a list of (index tensor axis)"
  (loop for index in *indices*
	when (> (count index dimensions :key #'first) 1)
	  collect `(assert (= ,@(remove-if #'not 
					   (mapcar (lambda (dims)
						     (if (eql (first dims) index)
							 `(array-dimension ,(second dims)
									   ,(third dims))))
						   dimensions))))))

(defun loop-over% (index expr then-function constant-product)
  (let ((*constant-indices* (cons index *constant-indices*)))
    (multiple-value-bind (const-expr remaining-expr) (extract-constant-expr expr)							     
      (if const-expr 
	  (let ((var (gensym "const-expr-")))
	    `(loop for ,(index-var index) from 0 below ,(index-max index) 
		   for ,var = ,(if constant-product
				   (expand-arefs `(* ,@const-expr ,constant-product))
				   (if (= (length const-expr) 1)
				       (expand-arefs (first const-expr))
				       (expand-arefs `(* ,@const-expr))))
		   ,@(funcall then-function remaining-expr var)))
	  `(loop for ,(index-var index) from 0 below ,(index-max index) 
		 ,@(funcall then-function remaining-expr constant-product))))))

(defun loop-over (index &key checking-constants-in then with-constant)
"Return a loop form taking care of any expression in `checking-constants-in' that can be taken out of the loop
(loop for index-var from 0 below index-max 
      for new-constant = (* with-constant ...) 
    ,@(then remaining-expr new-constant)"  
  (loop-over% index checking-constants-in then with-constant))


(defun einsum% (input-indices output-indices result-array &rest expr)
  (declare (optimize (debug 3)))
  (let* ((*indices* (map 'list #'identity input-indices))
	 (*indices-max* (mapcar (lambda (i) (gensym (concatenate 'string (string i) "-max"))) *indices*))
	 (*indices-vars* (mapcar (lambda (i) (gensym (string i))) *indices*))

	 (dimensions (walk-for-dimensions (cons '* expr)))
	 (result (if result-array result-array (gensym "result"))))

    ;; assign max-vars to size of indices
    `(let (,@(loop for index in *indices*
		   for max-var = (index-max index)
		   for dim = (find index dimensions :key #'first)
		   collect `(,max-var (array-dimension ,(second dim) ,(third dim)))))

       ;; dimension assertions
       ,@(dimension-assertions dimensions)

       ;; allocate resulting array or reuse given array 
       (let ,(unless result-array 
	       `((,result ,(if output-indices
			       `(make-array (list ,@(map 'list #'index-max output-indices)))))))
	 ;; now loop!! :) 
	 ,(labels ((outer-loop (indices expr const)
		     (loop-over (first indices)
				:with-constant const
				:checking-constants-in expr 
				:then (lambda (remaining-expr const)
					(if (> (length indices) 1)
					    `(do ,(outer-loop (rest indices)
							      remaining-expr
							      const))
					    (cond
					      ((and const remaining-expr) 
					       `(do (setf (aref ,result ,@(map 'list #'index-var output-indices))
							  (* ,const ,(inner-loop remaining-expr)))))
					      (remaining-expr
					       `(do (setf (aref ,result ,@(map 'list #'index-var output-indices)) 
								 ,(inner-loop remaining-expr))))
					      (const 
					       `(do (setf (aref ,result ,@(map 'list #'index-var output-indices))
							  ,const))))))))

		   (inner-loop% (indices expr const)
		     (loop-over (first indices)
				:checking-constants-in expr
				:with-constant const
				:then (lambda (remaining-expr const) 
					(if (> (length indices) 1)
					    (if const 
						`(summing (* ,const ,(inner-loop% (rest indices)
										  remaining-expr nil)))
						`(summing ,(inner-loop% (rest indices)
								       remaining-expr nil)))
					    (cond 
					      ((and const remaining-expr) 
					       `(summing (* ,const ,(expand-arefs `(* ,@remaining-expr)))))
					      (remaining-expr
					       `(summing ,(expand-arefs `(* ,@remaining-expr))))
					      (const 
					       `(summing ,const)))))))

		   (inner-loop (expr) 
		     (inner-loop% (set-difference *indices* (map 'list #'identity output-indices))
				  expr nil)))
	    (outer-loop (map 'list #'identity output-indices)
			expr 
			nil))
	 ;; return the results 
	 ,(if result-array 
	      result-array 
	      result)))))

(defmacro einsum ((rhs-indices to lhs-indices) &rest expr)
  "Expand expressions in Einstein's summation notation to loops"
  (assert (eql to :to))
  (if (eql (first expr) :into)
      (progn 
	(assert (symbolp (second expr)))
	(apply #'einsum% (string rhs-indices) (string lhs-indices) (second expr) (cddr expr)))
      (apply #'einsum% (string rhs-indices) (string lhs-indices) nil expr)))
