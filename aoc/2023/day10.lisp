(defconstant ground #b0000)
(defconstant north  #b0001)
(defconstant south  #b0010)
(defconstant east   #b0100)
(defconstant west   #b1000)

(defun char-to-dir (char)
  (ecase char
    (#\- (+ east west))
    (#\| (+ north south))
    (#\L (+ north east))
    (#\J (+ north west))
    (#\7 (+ south west))
    (#\F (+ south east))
    (#\. ground)
    (#\S ground)))

(defun inverse-dir (dir)
  (ecase dir
    (#.north south)
    (#.south north)
    (#.east west)
    (#.west east)))

(defun read-grid (stream)
  "Parse the input `stream' and return the grid cell values (as 1d array),
position of start cell and the grid size"
  (let ((arr (make-array 0 :element-type 'fixnum :fill-pointer 0 :adjustable t))
        (cols 0)
        (rows 0)
        start-loc)
    (loop for char = (read-char stream nil nil) do
      (cond ((eql char nil)
             (return))
            ((eql char #\Newline)
             (when (eql cols 0)
               (setf cols (length arr))))
            (t
             (when (eql char #\S)
               (setf start-loc (length arr)))
             (vector-push-extend (char-to-dir char) arr))))
    (setf rows (/ (length arr) cols))
    (values arr
            start-loc
            rows
            cols)))

(defun open? (grid pos dir)
  "Is the position `pos' in `grid' open from direction `dir'?"
  (not (= (logand dir (aref grid pos)) 0)))

(defun read-pipe-loop (stream)
  "Parse input `stream' and return a bitmap for pipe loop locations,
the underlying grid cell values and grid size"
  (multiple-value-bind (grid start-loc rows cols) (read-grid stream)
    (let ((pipe-bit-map (make-array (length grid) :element-type 'bit)))
      (labels ((new-loc (loc direction)
                 (ecase direction
                   (#.north (- loc cols))
                   (#.south (+ loc cols))
                   (#.east  (+ loc 1))
                   (#.west  (- loc 1))))

               (traverse (loc dir)
                 (setf (aref pipe-bit-map loc) 1)
                 (let* ((new-loc (new-loc loc dir)))
                   ;; unless we return back to same pos (i.e. completed a loop)
                   ;; traverse to next connected grid cell
                   (unless (= (aref pipe-bit-map new-loc) 1)
                     #+debug(assert (open? grid new-loc (inverse-dir dir)))
                     (let ((next-exit (- (aref grid new-loc)
                                         (inverse-dir dir))))
                       (traverse new-loc next-exit))))))

        (let (start-direction)
          ;; set the correct pipe junction value for start position
          (loop for dir in (list north south east west) do
            (when (open? grid
                         (new-loc start-loc dir)
                         (inverse-dir dir))
              (setf start-direction dir)
              (incf (aref grid start-loc) dir)))

          ;; start traversing from start-loc, marking connected pipes as 1
          (traverse start-loc start-direction)))
      (values pipe-bit-map grid rows cols))))

(defun solve1 ()
  (with-open-file (str "./problem/day10.txt")
    (let ((bitmap (read-pipe-loop str)))
      (/ (count 1 bitmap)
         2))))

(defun solve2 ()
  (with-open-file (str "./problem/day10.txt")
    (multiple-value-bind (bitmap grid rows cols) (read-pipe-loop str)
      (declare (ignore rows))
      (let ((inside-locations 0))

        (labels ((pipe? (pos)
                   (= (aref bitmap pos) 1))

                 (pipe-running-south? (pos)
                   (and (pipe? pos)
                        (open? grid pos south))))

          (loop for pos from 0 below (length bitmap)
                with intersections = 0 do
                  (when (= (mod pos cols) 0) ;; reset at the left edge
                    (setf intersections 0))

                  (when (pipe-running-south? pos)
                    (incf intersections))

                  (when (and (oddp intersections)
                             (not (pipe? pos)))
                    (incf inside-locations)))

          inside-locations)))))
