(in-package #:fem)

;; https://gmsh.info/doc/texinfo/gmsh.html#MSH-file-format

(defparameter *mesh* nil)
(defparameter *mesh-type* :3d)

(defstruct mesh
  (nodes)
  (elements))

(defmethod print-object ((o mesh) stream)
  (print-unreadable-object (o stream)
    (format stream "Nodes:~a, Elements:~a"
            (length (mesh-nodes o))
            (length (mesh-elements o)))))

(defmacro with-mesh-file ((file) &body body)
  `(with-open-file (*mesh* ,file :direction :input)
     ,@body))

(defmacro with-section (name &body body)
  `(let ((,name (handler-case (read-section-name)
                  (end-of-file (e)
                    (declare (ignore e))
                    nil))))
     ,@body))

(defun whitespace-charp (char)
  (or (char= char #\Space)
      (char= char #\Newline)
      (char= char #\Return)))

(defun read-nowhitespace-char ()
  (let ((char (read-char *mesh*)))
    (if (whitespace-charp char)
        (read-nowhitespace-char)
        char)))

(defun read-string ()
  (with-output-to-string (str)
    (loop for char = (read-char *mesh* nil nil) do
      (if (or (null char)
              (whitespace-charp char))
          (return)
          (write-char char str)))))

(defun read-section-name ()
  (let (($ (read-nowhitespace-char)))
    (unless (char= $ #\$)
      (error "not at start of section"))
    (read-string)))

(defun skip-section (name)
  (loop with end-marker = (format nil "$End~a" name)
        for line = (read-line *mesh*) do
          (when (string= line end-marker)
              (return))))

(defun end-section (name)
  (let ((end-marker (format nil "$End~a" name))
        (line (read-line *mesh*)))
    (cond ((every #'whitespace-charp line)
           (end-section name))
          ((string= line end-marker)
           t)
          (t
           (error "Section ~a doesn't end at ~a" name line)))))


(defun read-integer ()
  ;; TODO: implement checks for integer
  (read *mesh*))

(defun read-number ()
  ;; reads integers or double floats
  ;; TODO: same as above
  (read *mesh*))

(defmacro with-integers (names &body body)
  `(let (,@(loop for n in names
                 collect `(,n (read-integer))))
     ,@body))

(defun read-triangle ()
  (vector (read-integer) (read-integer) (read-integer)))

(defun read-node ()
  (ecase *mesh-type*
    (:3d (vector (read-number) (read-number) (read-number)))
    (:2d (prog1 (vector (read-number) (read-number))
           (read-number)))))

;; $Nodes
;;   numEntityBlocks(size_t) numNodes(size_t)
;;     minNodeTag(size_t) maxNodeTag(size_t)
;;   entityDim(int) entityTag(int) parametric(int; 0 or 1)
;;     numNodesInBlock(size_t)
;;     nodeTag(size_t)
;;     ...
;;     x(double) y(double) z(double)
;;        < u(double; if parametric and entityDim >= 1) >
;;        < v(double; if parametric and entityDim >= 2) >
;;        < w(double; if parametric and entityDim == 3) >
;;     ...
;;   ...
;; $EndNodes

(defun read-nodes ()
  (with-integers (num-blocks num-nodes node-min node-max)
    (declare (ignore num-nodes node-min))
    (let ((nodes (make-array (1+ node-max) :initial-element nil)))
      (loop repeat num-blocks do
        (with-integers (entity-dim entity-tag parametric num-nodes-in-block)
          (declare (ignore entity-dim entity-tag))
          (assert (= parametric 0))
          (let ((block-nodes (make-array num-nodes-in-block :fill-pointer 0)))
            ;; read block numbers
            (loop repeat num-nodes-in-block do
              (vector-push (read-integer) block-nodes))
            (loop for node-number across block-nodes do
              (setf (aref nodes node-number) (read-node))))))
      (end-section "Nodes")
      nodes)))


;; $Elements
;;   numEntityBlocks(size_t) numElements(size_t)
;;     minElementTag(size_t) maxElementTag(size_t)
;;   entityDim(int) entityTag(int) elementType(int; see below)
;;     numElementsInBlock(size_t)
;;     elementTag(size_t) nodeTag(size_t) ...
;;     ...
;;   ...
;; $EndElements
(defun read-elements ()
  (with-integers (num-blocks num-elements element-min element-max)
    (declare (ignore element-min element-max))
    (let ((elements (make-array num-elements :fill-pointer 0)))
      (loop repeat num-blocks do
        (with-integers (entity-dim entity-tag element-type num-elements)
          ;;(declare (ignore entity-dim entity-tag))
          (case element-type
            (15 ;; one node point
             (loop repeat num-elements do (read-integer) (read-integer)))
            (1 ;; 2 node line
             (loop repeat num-elements do (read-integer) (read-integer) (read-integer)))
            (2 ;; 3 node triangle
             (loop repeat num-elements do
               (read-number) ;; ignore element-number
               (vector-push (read-triangle) elements)))
            (t
             (print (list entity-dim entity-tag num-elements))
             (error "Can't handle element-type ~a" element-type)))))
      (end-section "Elements")
      elements)))

(defun cleanup-mesh (mesh)
  "Remove missing ids; and remove nodes that don't belong to any element"
  (with-slots (nodes elements) mesh
    (let ((cnodes (make-array (length nodes) :fill-pointer 0))
          (celements (make-array (length elements) :fill-pointer 0))
          (bitmap (make-array (length nodes) :element-type 'bit :initial-element 0)))
      (loop for e across elements do
            (map 'nil (lambda (n)
                        (setf (aref bitmap n) 1))
                 e))
      (loop for bit across bitmap
            for i from 0 do
            (when (= bit 0)
              (setf (aref nodes i) nil)))

      (loop for n across nodes
            for i* from 0
            with i = 0 do
              (when n
                (vector-push n cnodes)
                (setf (aref nodes i*) i)
                (incf i)))

      (loop for e across elements do
        (vector-push (map 'vector (lambda (node-i)
                                    (aref nodes node-i))
                          e)
                     celements))
      (make-mesh :nodes cnodes
                 :elements celements))))

(defun read-mesh (file)
  (declare (optimize (debug 3)))
  (with-mesh-file (file)
    (let (nodes elements)
      (loop do
        (with-section name
          (cond ((eql name nil)
                 (return))
                ((string= name "Nodes")
                 (setf nodes (read-nodes)))
                ((string= name "Elements")
                 (setf elements (read-elements)))
                (t (skip-section name)))))
      (cleanup-mesh (make-mesh :nodes nodes
                               :elements elements)))))

(defun read-2d-mesh (file)
  (let ((*mesh-type* :2d))
    (read-mesh file)))
