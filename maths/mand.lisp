(ql:quickload :lispbuilder-sdl)
(ql:quickload :cl-opengl)
(ql:quickload :lparallel)

(defparameter *width* 1200)
(defparameter *height* 700)
(defparameter *scale* 3e-3)
(defparameter *translation* (complex 0 0))

;; initialize 8 lparallel kernels 
(defparameter lparallel:*kernel* (lparallel:make-kernel 8))
(defparameter *regions* (let ((stepx (/ *width* 2))
			      (stepy (/ *height* 4)))
			  (loop for x0 from 0 to (- *width* stepx) by stepx
				with regions = nil do 
				  (loop for y0 from 0 to (- *height* stepy) by stepy
					do (push (mapcar (lambda (i) (truncate i))
							 (list x0 (+ x0 stepx)
							       y0 (+ y0 stepy)))
						 regions))
				finally (return regions))))

(deftype color ()
  '(unsigned-byte 8))
(defparameter *buffer-base* (make-array (* *height* *width* 3) :element-type 'color))
(defparameter *buffer* (make-array (list *height* *width* 3)
				   :element-type 'color
				   :displaced-to *buffer-base*))

;;; Mandelbrot Set Computations 

(defun iterate (c iterations limit)
  (declare (optimize (speed 3) (safety 0) (debug 0)))
  (declare ((complex single-float) c)
	   (fixnum iterations))
  (let ((f c))
    (declare ((complex single-float) f)
	     (single-float limit))
    (dotimes (iters iterations f)
      (setf f (+ (expt f 2) c))
      (when (> (abs f) limit)
	(return-from iterate iters)))
    nil))

(defun divergence-iters (c)
  "Number of iterations it took for `c' to diverge. NIL for `c' that belongs to madelbrot set"
  (iterate c
	   30
	   50.0))

(defun transform (x y)
  (declare (optimize (speed 3) (safety 0) (debug 0)))
  (declare (fixnum x y)
	   ((complex fixnum) *translation*)
	   (single-float *scale*))
  (+ *translation* (complex (* *scale* (the fixnum (- x 800)))
			    (* *scale* (the fixnum (- y 350))))))

(defun compute% (x0 x1 y0 y1) 
  (loop for x from x0 below x1 do
    (loop for y from y0 below y1 
	  for value =  (divergence-iters (transform x y)) do
	    (if value 
		;; when not in set, color the pixel
		(setf (aref *buffer* y x 0) (max 0 (min 255 (* 20 (abs value))))
		      (aref *buffer* y x 1) 0
		      (aref *buffer* y x 2) 0)
		;; when in set, just set to white color
		(setf (aref *buffer* y x 0) 0
		      (aref *buffer* y x 1) 0
		      (aref *buffer* y x 2) 0)))))

(defun compute ()
  (lparallel:pmap nil 
		  (lambda (region)
		    (apply #'compute% region))
		  *regions*))

;;; Drawing 
(defun draw ()
  (gl:draw-pixels *width* *height*
		  :rgb
		  :unsigned-byte
		  *buffer-base*))

(defun timing (function)
  (let ((t1 (get-internal-real-time)))
    (funcall function)
    (/ (- (get-internal-real-time) t1)
       internal-time-units-per-second)))

(defun main ()
  (sdl:with-init ()
    (sdl:window 1200 700 :resizable t :title-caption "Mandelbrot Set" :opengl t)
    (setf sdl:*default-color* sdl:*black*)
    (sdl:initialise-default-font)
    (sdl:enable-key-repeat 100 10)
    (sdl:with-events ()
      (:quit-event () t)
      (:key-down-event
       (:key key)
       (case key
	 (:sdl-key-q (sdl:push-quit-event))
	 (:sdl-key-l
	  (setf *scale* (* *scale* 1.2)))
	 (:sdl-key-k
	  (setf *scale* (/ *scale* 1.2)))
	 (:sdl-key-a
	  (incf *translation* (* *scale* #C(20 0))))
	 (:sdl-key-d
	  (decf *translation* (* *scale* #C(20 0))))
	 (:sdl-key-w
	  (incf *translation* (* *scale* #C(0 20))))
	 (:sdl-key-s
	  (decf *translation* (* *scale* #C(0 20))))))

      (:idle
       ()
       ;; Clear screen
       (sdl:clear-display sdl:*white*)
       ;; drawing
       (format t "~&Calculate : ~,3f sec" (timing #'calculate))
       (format t "~&Draw      : ~,3f sec" (timing #'draw))
       (sdl:update-display)
       ))))
