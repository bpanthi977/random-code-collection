(ql:quickload :lispbuilder-sdl)

(setf *read-default-float-format* 'double-float)
(deftype coord ()
  `(complex double-float))
(deftype color ()
  `sdl:color)

(defun dot (a b)
  "Dot product of coordinates a and b"
  (declare (type coord a b))
  (+ (* (realpart a) (realpart b))
     (* (imagpart a) (imagpart b))))

(defstruct system
  (balls nil :type (vector coord))
  (colors nil :type (vector color))
  (velocities nil :type (vector coord))
  (sphere-radius nil :type single-float)
  (balls-radius nil :type single-float))

(defparameter *system* nil)
(defparameter *gravity* #c(0.0 9.81))

(defun draw-circle (coord radius &optional (color sdl:*black*))
  (declare (type coord coord)
	   (type single-float radius)
	   (type color color))
  (let* ((scale 200)
	 (shift #c(400 250))
	 (coord (+ (* scale coord) shift)))
    (sdl:draw-circle-* (truncate (realpart coord))
		       (truncate (imagpart coord))
		       (truncate (* scale radius))
		       :color color)))
(defun draw (system)
  (declare (type system system))
  (loop for ball of-type coord  across (system-balls system)
	for color of-type color across (system-colors system)
	with radius = (system-balls-radius system) do
	  (draw-circle ball radius color))
  (draw-circle #c(0.0 0.0) (system-sphere-radius system))
  (values))

(defun draw-energy (system)
  (loop for ball of-type coord across (system-balls system)
	for velocity of-type coord across (system-velocities system)
	summing (+ (* 1/2 (expt (abs velocity) 2))
		   (- (dot *gravity* ball)))
	  into energy
	finally (sdl:draw-string-solid-* (format nil "energy: ~0,3f" energy)
					 8 16)))
	  

(defun update-state (dt position velocity radius sphere-radius)
  (declare (type coord position velocity)
	   (type single-float radius sphere-radius))
  (let ((|p| (abs position)))
    (cond ((< (+ |p| radius) sphere-radius)
	   ;; no collision
	   (values (+ position (* dt velocity) (* 1/2 *gravity* dt dt))
		   (+ velocity (* dt *gravity*))))
	  (t ;; collision 
	   (let* ((normal-dir (/ position |p|))
		  (dot (dot velocity normal-dir))
		  (normal (* normal-dir dot))
		  (parallel (- velocity normal))
		  (shifted-position (* position (- sphere-radius radius) (/ |p|))))
	     (flet ((correction (velocity* del-pe)
		      ;; correction for energy change due to shift in position
		      ;; within gravitational field
		      ;; 1/2 v1^2 + gh1 = 1/2 v2^2 + gh2 
		      (* velocity* (sqrt (- 1 (/ (* 2 del-pe)
						 (expt (abs velocity*) 2)))))))
	       (values shifted-position
		       (correction (+ (- normal) parallel)
				   (dot *gravity* (- (- shifted-position position)))))))))))

(defun update (dt system)
  (declare (type float dt)
	   (type system system))
  (loop for position of-type coord across (system-balls system)
	for i from 0 
	for velocity of-type coord across (system-velocities system)
	with radius = (system-balls-radius system) do
	  (multiple-value-bind (position velocity)
	      (update-state dt position velocity radius (system-sphere-radius system))
	    (setf (aref (system-balls system) i) position
		  (aref (system-velocities system) i) velocity)))
  system)

(defun coords (&rest complex-coordinates)
  (assert (every (lambda (c) (typep c 'coord)) complex-coordinates))
  (make-array (length complex-coordinates)
	      :element-type 'coord
	      :initial-contents complex-coordinates))

(defun initialize-system (&optional (type :single))
  (ecase type 
    (:single (make-system :balls (coords #c(0.0000001 0.0) #C(0.0 0.0) #C(0.00000001 0.0))
			  :colors (vector sdl:*red* sdl:*blue* sdl:*green*)
			  :velocities (coords #C(0.0 0.0) #C(0.0 0.0) #C(0.0 0.0))
			  :sphere-radius 1.0f0
			  :balls-radius 0.05f0))
    ))

(defun speedup (dt)
  (* 1 (float dt)))

(defun main ()
  (sdl:with-init ()
    (sdl:window 800 500 :resizable t :title-caption "Chaotic Balls in Sphere ")
    (setf sdl:*default-color* sdl:*black*)
    (sdl:initialise-default-font)
    (let ((system (initialize-system))
	  (t1 (get-internal-real-time)))
      (sdl:with-events ()
	(:quit-event () t)
	(:key-down-event (:key key)
			 (case key
			   (:sdl-key-q (sdl:push-quit-event))))	
	
	(:idle ()
               ;; Clear screen
               (sdl:clear-display sdl:*white*)
	       (draw-energy system)
               ;; Update physics
	       (let* ((t2 (get-internal-real-time))
		     (dt (speedup (/ (- t2 t1) internal-time-units-per-second))))
		 (loop repeat 10
		       do (update (/ dt 10) system))
		 (setf t1 t2))
               ;; Draw objects
               (draw system)

               ;; Update screnn
               (sdl:update-display))))))
