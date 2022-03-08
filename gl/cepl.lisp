;;(ql:quickload '(:cepl.sdl2 :nineveh :livesupport))
(defpackage #:cepl-package 
  (:use #:cl 
	#:cepl
	#:rtg-math
	#:varjo
	#:nineveh))

(in-package :cepl-package)

(defvar *gpu-verts-arr* nil)
(defvar *gpu-index-arr* nil)
(defvar *vert-stream* nil)


(defun-g draw-verts-vert-stage ((vert :vec2))
  (v! vert 0.0 1.0))

(defun-g draw-verts-frag-stage ()
  (v! 0 1 0 0))

(defpipeline-g draw-verts-pipeline () 
  (draw-verts-vert-stage :vec2)
  (draw-verts-frag-stage))

(defun draw! () 
  (livesupport:continuable 
    (step-host)
    (livesupport:update-repl-link)
    (clear)
    (map-g #'draw-verts-pipeline *vert-stream*)
    (swap)))

(defun init () 
  (when *gpu-verts-arr*
    (free *gpu-verts-arr*))

  (when *gpu-index-arr*
    (free *gpu-index-arr*))

  (setf *gpu-verts-arr* 
	(make-gpu-array 
	 (list (v! -0.5 0.5)
	       (v! -0.5 -0.5)
	       (v!  0.5 -0.5)
	       (v!  0.5  0.5))
	 :element-type :vec2))

  (setf *gpu-index-arr*
	(make-gpu-array 
	 (list 0 1 2 0 2 3)
	 :dimensions 6
	 :element-type :int))
  
  (setf *vert-stream* 
	(make-buffer-stream *gpu-verts-arr*)))
			    

