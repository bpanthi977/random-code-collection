(ql:quickload :lispbuilder-sdl)
(in-package #:fem)

(defun transform-node (node)
  (let ((scale 600))
    (sdl:point :x (+ 100 (truncate (* scale (x node))))
               :y (- 650 (truncate (* scale (y node)))))))

(defun draw-mesh (mesh)
  (let ((nodes (mesh-nodes mesh)))
    (flet ((element-nodes (e)
             (list (aref nodes (aref e 0))
                   (aref nodes (aref e 1))
                   (aref nodes (aref e 2))))

           (draw-line (node1 node2)
             (sdl:draw-line (transform-node node1)
                            (transform-node node2))))

      (loop for e across (mesh-elements mesh)
            for nodes = (element-nodes e) do
              (draw-line (first nodes) (second nodes))
              (draw-line (second nodes) (third nodes))
              (draw-line (third nodes) (first nodes))))))

(defun show-mesh (mesh)
  (sdl:with-init ()
    (sdl:window 1200 700 :resizable t :title-caption "Mesh Viewer")
    (setf sdl:*default-color* sdl:*black*)
    (sdl:initialise-default-font)

    (sdl:clear-display sdl:*white*)
    (draw-mesh mesh)
    (sdl:update-display)
    (sdl:with-events ()
      (:quit-event () t)
      (:key-down-event
       (:key key)
       (cond ((eql key :sdl-key-q)
              (sdl:push-quit-event)))))))


(defun displaced-mesh (mesh solution)
  (let ((newnodes (make-array (length (mesh-nodes mesh)))))
    (loop for n across (mesh-nodes mesh)
          for i from 0 do
            (setf (aref newnodes i)
                  (vector (+ (aref n 0) (aref solution (* i 2)))
                          (+ (aref n 1) (aref solution (1+ (* i 2)))))))
    (make-mesh :nodes newnodes
               :elements (mesh-elements mesh))))

(defun draw-solution (mesh solution)
  (let ((sdl:*default-color* sdl:*red*))
    (draw-mesh (displaced-mesh mesh solution))))

(defun show-solution (mesh solution)
  (sdl:with-init ()
    (sdl:window 1200 700 :resizable t :title-caption "Solution Viewer")
    (setf sdl:*default-color* sdl:*black*)
    (sdl:initialise-default-font)

    (sdl:clear-display sdl:*white*)
    (draw-mesh mesh)
    (draw-solution mesh solution)
    (sdl:update-display)
    (sdl:with-events ()
      (:quit-event () t)
      (:key-down-event
       (:key key)
       (cond ((eql key :sdl-key-q)
              (sdl:push-quit-event)))))))

(defun solve-and-show (problem)
  (let ((solution (solve problem)))
    (show-solution (make-mesh :nodes (problem-nodes problem)
                              :elements (problem-elements problem))
                   (grid:cl-array solution))))
