(ql:quickload :lispbuilder-sdl)

(setf *read-default-float-format* 'double-float)

;; Utilities (Ring Buffer)
(defstruct ring-buffer
  (buffer nil :type (vector coord))
  (fill-pointer nil :type fixnum)
  (count nil :type fixnum))

(defun initialize-ring-buffer (size)
  (assert (not (= size 0)))
  (make-ring-buffer :buffer (make-array size :element-type 'coord :initial-element #C(0.0 0.0))
		    :fill-pointer 0
		    :count 0))

(defun ring-buffer-push (item ring)
  (with-slots (buffer fill-pointer count) ring
    (let* ((size (length buffer)))
      (setf (aref buffer fill-pointer) item
	    fill-pointer (mod (1+ fill-pointer) size)
	    count (min size (1+ count))))
    ring))

(defun map-ring-buffer (function ring)
  (with-slots (buffer fill-pointer count) ring
    (unless (= count 0)
      (loop with size = (length buffer)
	    for i from (1- fill-pointer) downto (- fill-pointer count) do
	      (funcall function (aref buffer (mod i size)))))))

(defun dot (a b)
  "Dot product of coordinates a and b"
  (declare (type coord a b))
  (+ (* (realpart a) (realpart b))
     (* (imagpart a) (imagpart b))))

;;;
;;; Data Structures
;;;
(deftype coord ()
  `(complex double-float))

(deftype color ()
  `sdl:color)

(defstruct system
  (balls nil :type (vector coord))
  (trails nil :type (vector ring-buffer))
  (colors nil :type (vector color))
  (velocities nil :type (vector coord))
  (sphere-radius nil :type single-float)
  (balls-radius nil :type single-float))

(defparameter *gravity* #c(0.0 9.81))

;;;
;;; Drawing
;;;
(defparameter *scale* 200)
(defparameter *shift* #C(400 250))

(defun transform (coord)
  (declare (type coord coord))
  (+ (* *scale* coord) *shift*))

(defun coord->sdl-point (coord)
  (declare (type coord coord))
  (sdl:point :x (truncate (realpart coord))
	     :y (truncate (imagpart coord))))

(defun draw-circle (coord radius &optional (color sdl:*black*))
  (declare (type coord coord)
	   (type single-float radius)
	   (type color color))
  (let* ((coord (transform coord)))
    (sdl:draw-circle-* (truncate (realpart coord))
		       (truncate (imagpart coord))
		       (truncate (* *scale* radius))
		       :color color)))

(defun draw-trail (start ring-buffer color)
  (declare (type coord start)
	   (type ring-buffer ring-buffer)
	   (type color color))
  (let ((start (coord->sdl-point (transform start))))
    (map-ring-buffer (lambda (end)
		       (setf end (coord->sdl-point (transform end)))
		       (sdl:draw-line start end :color color)
		       (setf start end))
		     ring-buffer)))

(defun draw (system)
  "Draws everything i.e. some circles"
  (declare (type system system))
  (loop for ball of-type coord  across (system-balls system)
	for trail of-type ring-buffer across (system-trails system)
	for color of-type color across (system-colors system)
	with radius = (system-balls-radius system) do
	  (draw-trail ball trail color)
	  (draw-circle ball radius color))
  (draw-circle #c(0.0 0.0) (system-sphere-radius system))
  (values))

(defun draw-energy (system)
  "Total energy of system (For debug purpose) should stay constant for good physics integrator"
  (declare (type system system))
  (loop for ball of-type coord across (system-balls system)
	for velocity of-type coord across (system-velocities system)
	summing (+ (* 1/2 (expt (abs velocity) 2))
		   (- (dot *gravity* ball)))
	  into energy
	finally (sdl:draw-string-solid-* (format nil "energy: ~0,3f" energy)
					 8 16)))
;;;
;;; Physics
;;;

(defun update-state (dt position velocity radius sphere-radius)
  "Physics update for a single ball"
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
  "Physics update for whole system"
  (declare (type float dt)
	   (type system system))
  (loop for position of-type coord across (system-balls system)
	for i from 0 
	for velocity of-type coord across (system-velocities system)
	for trail of-type ring-buffer across (system-trails system)
	with radius = (system-balls-radius system) do
	  (multiple-value-bind (position velocity)
	      (update-state dt position velocity radius (system-sphere-radius system))
	    (setf (aref (system-balls system) i) position
		  (aref (system-velocities system) i) velocity)
	    (ring-buffer-push position trail)))
  (values))

;;;
;;; Initialization/Main
;;; 


(defun coords (&rest complex-coordinates)
  "Returns a vector of coordinates"
  (assert (every (lambda (c) (typep c 'coord)) complex-coordinates))
  (make-array (length complex-coordinates)
	      :element-type 'coord
	      :initial-contents complex-coordinates))

(defun initialize-system ()
  (make-system :balls (coords #C(0.0000001 0.0) #C(0.0 0.0) #C(0.00000001 0.0))
	       :colors (vector sdl:*red* sdl:*blue* sdl:*green*)
	       :velocities (coords #C(0.0 0.0) #C(0.0 0.0) #C(0.0 0.0))
	       :trails (make-array 3 :initial-contents (loop repeat 3 collect (initialize-ring-buffer 100)))
	       :sphere-radius 1.0f0
	       :balls-radius 0.05f0))

(defun speedup (dt)
  "Change speed of simulation. (For debug purpose)"
  dt)

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
               ;; Update physics
	       (let* ((t2 (get-internal-real-time))
		      (dt (float (speedup (/ (- t2 t1) internal-time-units-per-second)))))
		 (loop repeat 10
		       do (update (/ dt 10) system))
		 (setf t1 t2))
               ;; Draw objects
               (draw system)
	       (draw-energy system)
               ;; Update screnn
               (sdl:update-display))))))
