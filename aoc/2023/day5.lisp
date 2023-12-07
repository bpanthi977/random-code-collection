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

(defun read-numbers (stream &optional (n 0))
  "Read `n' numbers separated by any single character or multiple spaces
If `n' = 0, read indefinitely"
  (loop with numbers = (make-array 0 :adjustable t :fill-pointer 0)
        for number = (read-number stream)
        do
           (if number
               (progn (vector-push-extend number numbers)
                      (when (= (length numbers) n)
                        (return numbers)))
               (return numbers))))

(defun read-number-tuples (stream n)
  "Reads as many `n'-tuples of numbers as it can from `stream'"
  (loop for row = (read-numbers stream n)
        with rows = (list)
        do (if (> (length row) 0)
               (push row rows)
               (return rows))))

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
