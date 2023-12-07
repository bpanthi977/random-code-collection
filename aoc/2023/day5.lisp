;;; UTILS

(defun read-until (stream char)
  "Move the `stream' beyond `char'"
  (loop for c = (read-char stream)
        when (char= c char)
          return t))

(defun read-number (stream)
  "Read a number from `stream'
Ignores preceeding space but stops at any other character."
  (loop with number = 0
        with found = nil
        for c = (read-char stream nil #\Newline) do
          (cond ((digit-char-p c)
                 (setf number (+ (* number 10)
                                 (- (char-code c)
                                    #.(char-code #\0)))
                       found t))
                ((char= c #\Space)
                 (if found (return number)))
                (t
                 (return (and found number))))))

(deftype u64 () `(unsigned-byte 64))
(defun read-numbers (stream &optional (n 0))
  "Read `n' numbers separated by any single character or multiple spaces
If `n' = 0, read indefinitely"
  (declare (optimize (speed 3))
           (type u64 n))
  (let ((numbers (make-array 0 :element-type 'u64 :adjustable t :fill-pointer 0)))
    (loop for number = (read-number stream)
          do
             (if number
                 (progn (vector-push-extend number numbers)
                        (when (= (length numbers) n)
                          (return numbers)))
                 (return numbers)))
    (make-array (length numbers) :element-type 'u64 :initial-contents numbers)))

(defun read-number-tuples (stream n)
  "Reads as many `n'-tuples of numbers as it can from `stream'"
  (declare (type u64 n))
  (the (cons (simple-array u64))
       (loop for row = (read-numbers stream n)
             with rows = (list)
             do (if (> (length row) 0)
                    (push row rows)
                    (return rows)))))

;;; PART 1

(defun read-map (str)
  "Create a map that translates source number to destination number"
  (read-until str #\:)
  (read-until str #\Newline)
  (let* ((rows (read-numbers str))
         (count (/ (length rows) 3)))

    (lambda (source)
      (loop for idx from 0 below count
            for dest-start   = (aref rows (+ (* idx 3) 0))
            for source-start = (aref rows (+ (* idx 3) 1))
            for diff         = (aref rows (+ (* idx 3) 2)) do
              (when (<= source-start source (+ source-start diff))
                (return (+ dest-start (- source source-start))))
            finally (return source)))))

(defun solve1 ()
  (with-open-file (str "./problem/day5.txt")
    (let ((seeds (progn (read-until str #\:)
                        (read-numbers str)))
          (maps (loop repeat 7
                      collect (read-map str))))
      (flet ((seed-to-location (seed)
               (reduce (lambda (source map) (funcall map source))
                       maps
                       :initial-value seed)))
        (reduce #'min (map 'vector #'seed-to-location seeds))))))


;;; PART 2

(defun read-map2 (str)
  "Creates a function M(s,e) that takes source start and end, and returns
a destination start and end such that  destination end - start is the
max consecutive number of source that can be translated to destination.

E.g. if see-to-soil map translates
  (1 ... 10) => (51 ... 60) and (11 ... 20) => (101, 110)
then
  M(5, 15) = (55, 60)"
  (read-until str #\:)
  (read-until str #\Newline)
  (let* ((rows (sort
                (read-number-tuples str 3)
                #'<
                :key (lambda (row) (elt row 1)))))

    (lambda (start end)
      (loop for row in rows
            for dest-start = (elt row 0)
            for source-start = (elt row 1)
            for diff = (elt row 2) do
              (cond ((<= source-start start (+ source-start diff -1))
                     (return (list
                              (+ dest-start (- start source-start))
                              (min (+ dest-start (- end source-start))
                                   (+ dest-start diff)))))
                    ((< start source-start)
                     (return (list
                              start
                              (min (- source-start 1) end)))))
            finally (return (list start end))))))


(defun solve2 ()
  (with-open-file (str "./problem/day5.txt")
    (let ((seeds (sort (progn (read-until str #\:)
                              (read-number-tuples str 2))
                       #'<
                       :key (lambda (pair) (elt pair 0))))
          (maps (loop repeat 7
                      collect (read-map2 str))))
      (flet ((seed-to-location (start end)
               (loop for map in maps
                     for (s e) = (funcall map start end) do
                       (setf start s
                             end e)
                     finally (return (list s e)))))
        (loop with min-loc = most-positive-fixnum
              for pair in seeds
              for start = (elt pair 0)
              for end = (+ start (elt pair 1) -1)
              do
                 (loop with seed = start
                       for (dst-start dst-end)
                         = (seed-to-location seed end)
                       do
                          (setf seed (+ seed (max (- dst-end dst-start)
                                                  1)))
                          (when (< dst-start min-loc)
                            (setf min-loc dst-start))
                          (when (>= seed end)
                            (return)))
              finally (return min-loc))))))

;;; Part 2 Bruteforce

(defstruct translation
  (dest 0 :type u64)
  (start 0 :type u64)
  (end 0 :type u64))

(defun read-translation-map (str)
  "Create a map that translates source number to destination number"
  (declare (optimize (speed 0) (safety 3) (debug 3)))
  (read-until str #\:)
  (read-until str #\Newline)
  (map '(vector translation)
       (lambda (vec)
         (make-translation :dest (elt vec 0)
                           :start (elt vec 1)
                           :end (+ (elt vec 1) (elt vec 2))))
       (read-number-tuples str 3)))

(declaim (inline seed-to-location))
(defun seed-to-location (seed maps)
  (declare (type u64 seed)
           (type (simple-array (simple-array translation)) maps)
           (optimize speed (safety 0)))
  (loop for map of-type (simple-array translation) across maps do
    (loop for transt of-type translation across map do
      (when (and (<= (translation-start transt) seed)
                 (< seed (translation-end transt)))
        (let ((transformed (+ (translation-dest transt)
                             (the u64 (- seed (translation-start transt))))))
          (declare (type u64 transformed))
          (setf seed transformed))
        (return))))
  seed)

(defun solve2-bruteforce ()
  (declare (optimize speed (safety 0)))
  (with-open-file (str "./problem/day5.txt")
    (let ((ranges (progn (read-until str #\:)
                         (read-number-tuples str 2)))
          (maps (make-array
                 7
                 :initial-contents (loop repeat 7
                                         collect (read-translation-map str))
                 :element-type '(simple-array translation))))
      (declare (type (cons (simple-array u64)) ranges))
      (loop with min-loc of-type u64 = most-positive-fixnum
            for range of-type (simple-array u64) in ranges
            for start of-type u64 = (aref range 0)
            for end of-type u64 = (+ start (aref range 1)) do
              (loop for seed of-type u64 from start below end
                    for loc of-type u64 = (seed-to-location seed maps) do
                      (when (< loc min-loc)
                        (setf min-loc loc)))
            finally (return min-loc)))))
