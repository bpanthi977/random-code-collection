;; for einsum notation
(load #p"~/lisp/rcc/ml/einstein.lisp")

(defpackage #:fem
  (:use #:cl #:einsum))

(in-package #:fem)
;; linear algebra utilities
(load "./parse-mesh.lisp")
(load "./fastlinalg.lisp")

(defstruct material
  (e)
  (nu))

(defstruct problem
  (material)
  (nodes)
  (elements)
  (thickness)
  (constraints)
  (loads))

(defmethod print-object ((o problem) stream)
  (print-unreadable-object (o stream)
    (format stream "Nodes:~a, Elements:~a, Materials: ~a, Constraints: ~a, Loads: ~a"
            (length (problem-nodes o))
            (length (problem-elements o))
            (problem-material o)
            (length (problem-constraints o))
            (length (problem-loads o)))))

(defun make-vector (tree)
  "Returns a Vector of vectors from given tree"
  (let ((vector (make-array (length tree) :fill-pointer 0)))
    (loop for el in tree do
      (vector-push (if (listp el)
                       (apply #'vector el)
                       el)
                   vector))
    vector))

(defun parse-input-file (&optional (file #p"./simple-mesh-input"))
  (let ((input (uiop:read-file-forms file)))
    (make-problem :material (apply #'make-material (getf input :material))
                  :nodes (make-vector (getf input :nodes))
                  :elements (make-vector (getf input :elements))
                  :constraints (getf input :constraints)
                  :thickness (getf input :thickness)
                  :loads (getf input :loads))))

(defparameter *problem* nil)

(defun node (node-index)
  (aref (problem-nodes *problem*) node-index))

(defun x (node)
  (aref node 0))

(defun y (node)
  (aref node 1))

(defun nodes-count ()
  (length (problem-nodes *problem*)))

(defmethod elasticity-matrix ((material material))
  (let* ((nu (material-nu material))
         (d* (make-array '(3 3) :initial-contents
                         `((1 ,nu 0)
                           (,nu 1 0)
                           (0 0 ,(/ (- 1 nu) 2)))))
         (multiplier (/ (material-e material)
                        (- 1 (expt nu 2)))))
    (einsum (ij :to ij)
            :into d* (* multiplier (ij d*)))))

(defmethod elasticity-matrix ((p problem))
  (elasticity-matrix (problem-material p)))

(defun c-matrix (element)
  (let ((c (make-array '(3 3))))
    (loop for node-index across element
          for node = (node node-index)
          for x = (aref node 0)
          for y = (aref node 1)
          for j from 0 do
            (setf (aref c j 0) 1
                  (aref c j 1) x
                  (aref c j 2) y))
    c))

(defun B-matrix (element)
  (let* ((c (c-matrix element))
         (c-inv (invert c)))
    (flet ((delN/x (i x)
             (case x
               (:x (grid:aref c-inv 1 i))
               (:y (grid:aref c-inv 2 i)))))
      (make-array '(3 6) :initial-contents
                  (list (list (delN/x 0 :x) 0 (delN/x 1 :x) 0 (delN/x 2 :x) 0)
                        (list 0 (delN/x 0 :y) 0 (delN/x 1 :y) 0 (delN/x 2 :y))
                        (list (delN/x 0 :y) (delN/x 0 :x)
                              (delN/x 1 :y) (delN/x 1 :x)
                              (delN/x 2 :y) (delN/x 2 :x)))))))

(defun interpolation-function (element deflections)
  (let* ((c (c-matrix element))
         (c-inv (invert c))
         (u (make-array 3 :initial-contents (list (aref deflections 0)
                                                  (aref deflections 2)
                                                  (aref deflections 4))))
         (v (make-array 3 :initial-contents (list (aref deflections 1)
                                                  (aref deflections 3)
                                                  (aref deflections 5))))
         (c1 (einsum (ij :to i)
                     (* (ij c-inv) (j u))))
         (c2 (einsum (ij :to i)
                     (* (ij c-inv) (j v)))))
    (lambda (x y)
      (let ((tmp (make-array 3 :initial-contents (list 1 x y))))
        (values (reduce #'+ (map 'vector #'* c1 tmp))
                (reduce #'+ (map 'vector #'* c2 tmp)))))))

(defun stiffness-matrix (element)
  (let* ((b (b-matrix element))
         (d (elasticity-matrix (problem-material *problem*)))
         (c (c-matrix element))
         (area (abs (* 1/2 (determinant c))))
         (thickness (problem-thickness *problem*))
         (db (matmul d b (* area thickness))))
    ;; k = B' * (D * B) * det(C)/2 * thickness
    ;; B is 3x6 matrix, D is 3x3 matrix
    ;; so k is 6*6 matrix
    (matmul (transpose b) db)))

(defun create-global-stiffness-matrix ()
  (let* ((n (nodes-count))
         (global-k (make-array (list (* 2 n) (* 2 n)) :initial-element 0)))
    (flet ((add-k (i j xi xj value)
             (incf (aref global-k
                         (if (eql xi :x) (* 2 i) (1+ (* 2 i)))
                         (if (eql xj :x) (* 2 j) (1+ (* 2 j))))
                 value)))
      (loop for element across (problem-elements *problem*)
            for k = (stiffness-matrix element) do

              (loop for i across element ;; global index of node
                    for i* from 0  ;; local index of node
                    do (loop for j across element
                             for j* from 0 do
                               (add-k i j :x :x (grid:aref k (* 2 i*) (* 2 j*)))
                               (add-k i j :x :y (grid:aref k (* 2 i*) (1+ (* 2 j*))))
                               (add-k i j :y :x (grid:aref k (1+ (* 2 i*)) (* 2 j*)))
                               (add-k i j :y :y (grid:aref k (1+ (* 2 i*)) (1+ (* 2 j*)))))))
      global-k)))

(defun apply-constraints (global-k)
  ;; todo check that loads are not applied in constrained directions on nodes
  (let ((n (length (problem-nodes *problem*))))
    (flet ((zero-out-row-and-col (diag)
             (loop for ij from 0 below (* 2 n) do
               (if (= ij diag)
                   (setf (aref global-k ij ij) 1)
                   (setf (aref global-k diag ij) 0
                         (aref global-k ij diag) 0)))))

      (loop for constraint in (problem-constraints *problem*)
            for (node type) = constraint do
              (cond ((eql type :x)
                     (zero-out-row-and-col (* 2 node)))
                    ((eql type :y)
                     (zero-out-row-and-col (1+ (* 2 node))))
                    ((eql type :xy)
                     (zero-out-row-and-col (* 2 node))
                     (zero-out-row-and-col (1+ (* 2 node))))
                    (t (error "Unknown constraint type ~a" type))))
      global-k)))

(defmacro with-problem ((problem) &body body)
  `(let ((*problem* ,problem))
     ,@body))

(defun solve (problem)
  (with-problem (problem)
    (let* ((K (apply-constraints (create-global-stiffness-matrix)))
           (n (length (problem-nodes problem)))
           (f (make-array (* 2 n) :initial-element 0)))
      (loop for (node load) in  (problem-loads problem)
            for (fx fy) = load do
              (setf (aref f (* 2 node)) fx
                    (aref f (1+ (* 2 node))) fy))
      (solve-lineqn K f))))

(defun mesh-problem ()
  (flet ((dist (node x y)
           (+ (expt (- (x node) x) 2)
              (expt (- (y node) y) 2))))
    (let ((mesh (read-2d-mesh #p"~/untitled.msh")))
      (make-problem :material (make-material :e 2000
                                             :nu 0.3)
                    :nodes (mesh-nodes mesh)
                    :elements (mesh-elements mesh)
                    :thickness 0.1
                    :constraints
                    ;; all bottom nodes are fixed
                    (loop for n across (mesh-nodes mesh)
                          for index from 0
                          when (and n (= (y n) 0))
                            collect (list index :xy))
                    :loads
                    ;; nodes near the top have a slight left direction load
                    (loop for n across (mesh-nodes mesh)
                          for index from 0
                          when (and n (< (dist n 0.01 0.99) 0.01))
                            collect (list index (list (/ -.1 131) 0)))))))
