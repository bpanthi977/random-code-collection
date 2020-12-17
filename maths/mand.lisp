(ql:quickload :lispbuilder-sdl)
(ql:quickload :lparallel)

(defparameter *scale* 0.2)
(defparameter *translation* (complex -200 -300))
(defparameter *memory* (make-hash-table :test #'eql))
(defparameter lparallel:*kernel* (lparallel:make-kernel 8))
(deftype color ()
  '(integer 0 256))
(defparameter *buffer* (make-array (list 1201 701 3)
				   :element-type 'color
				   :initial-element 0))


(defun iterate (c iterations)
  (declare (optimize (speed 3) (safety 0) (debug 0)))
  (declare ((complex single-float) c)
	   (fixnum iterations))
  (let ((f c)
	(limit 500))
    (declare ((complex single-float) f)
	     (fixnum limit))
    (dotimes (iters iterations f)
      (setf f (+ (expt f 2) c))
      (when (> (abs f) limit)
	(return-from iterate iters)))
    nil))


(defun belongs-to-mandelbrot (c)
  (iterate c
	   30))

(defun translate (x y)
  (declare (optimize (speed 3) (safety 0) (debug 0)))
  (declare (fixnum x y)
	   ((complex fixnum) *translation*)
	   (single-float *scale*))
  (complex (* *scale* (the fixnum (+ (realpart *translation*) x)))
	   (* *scale* (the fixnum (+ (imagpart *translation*) y)))))

(defun calculate% (x0 x1 y0 y1) 
  (loop for x from x0 below x1 do
    (loop for y from y0 below y1 
	  for value =  (belongs-to-mandelbrot (translate x y)) do
	    (if value 
	      ;; (sdl:draw-pixel-* x y :color
	      ;;  			(sdl:color :g (max 0 (min 255 (- (* 2 value)))) :b (min 255 (* 10 value)) :r (min  255 (* 20 value))))
	      (setf (aref *buffer* x y 0) (min  255 (* 20 value))
		    (aref *buffer* x y 1) (max 0 (min 255 (- (* 2 value))))
		    (aref *buffer* x y 2) (min 255 (* 10 value)))
	      (setf (aref *buffer* x y 0) 0
		    (aref *buffer* x y 1) 0
		    (aref *buffer* x y 2) 0))
	  ))))

(defun calculate (&aux
		(stepx (/ 1200 2))
		(stepy (/ 700 4)))
  (lparallel:pmap nil (lambda (region)
			(apply #'calculate% region))
		  (loop for x0 from 0 to (- 1200 stepx) by stepx
			with regions = nil do 
			  (loop for y0 from 0 to (- 700 stepy) by stepy
				do (push (mapcar (lambda (i) (truncate i))
						 (list x0 (+ x0 stepx)
						       y0 (+ y0 stepy)))
					 regions))
			finally (return regions))))

(defun draw ()
  (loop for x from 0 to 1200 do
    (loop for y from 0 to 700 do
      (sdl:draw-pixel-* x y :color (sdl:color :r (aref *buffer* x y 0)
					      :g (aref *buffer* x y 1)
					      :b (aref *buffer* x y 2))))))

(defun timing (function)
  (let ((t1 (get-internal-real-time)))
    (funcall function)
    (/ (- (get-internal-real-time) t1)
       internal-time-units-per-second)))

(defun main ()
  (sdl:with-init ()
    (sdl:window 1200 700 :resizable t :title-caption "Mandelbrot Set")
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
	  (incf *translation* #C(20 0)))
	 (:sdl-key-d
	  (decf *translation* #C(20 0)))
	 (:sdl-key-w
	  (incf *translation* #C(0 20)))
	 (:sdl-key-s
	  (decf *translation* #C(0 20)))))

      (:idle
       ()
       ;; Clear screen
       (sdl:clear-display sdl:*white*)
       ;; drawing
       (format t "~&Calculate : ~,3f sec" (timing #'calculate))
       (format t "~&Draw      : ~,3f sec" (timing #'draw))
       (sdl:update-display)
       ))))
