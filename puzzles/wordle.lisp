(ql:quickload :serapeum)
(ql:quickload :lparallel)
(setf lparallel:*kernel* (lparallel:make-kernel 8))

;;; Word List
(defparameter *answers* (uiop:read-file-lines "./wordle-answers-alphabetical.txt"))
(defparameter *words* (concatenate 'vector
                                   *answers*
                                   (uiop:read-file-lines "./wordle-allowed-guesses.txt")))

(declaim (type (simple-vector) *words*))
(defconstant +max-answer-word-index+ (1- (length *answers*)))
(defconstant +max-index+ (1- (length *words*)))

;;; Index
;; instead of string, the index of a word in the *words* vector is used to represent
;;  the word in the program. so, we define few things to ease that

(deftype index ()
  `(integer 0 ,+max-index+))

(defun index (string)
  "return index of `string'"
  (position string *words* :test #'string-equal))

(defun index->word (index)
  "return word at the `index'"
  (aref *words* index))

;;; Color
(deftype color ()
  '(integer 0 #.(1- (expt 3 5))))

(declaim (ftype (function (string string) color) color0))
(defun color0 (guess actual-word)
  "the sequence of colors obtained when `guess' word is used to guess `actual-word'
represented as a number for 0 to 3^5 in base 3
| digit | meaning         |
|-------|-----------------|
| 0     | wrong           |
| 1     | different place |
| 2     | right           |"
  (let ((total 0)
        (matches (make-array 5 :element-type 'bit :initial-element 0)))
    (flet ((match-position (char)
             (loop for c across actual-word
                   for pos from 0
                   when (and (eql c char) (eql (aref matches pos) 0))
                     return pos)))

      ;; mark green positions
      (map-into matches (lambda (c1 c2) (if (eql c1 c2) 1 0))
                guess actual-word)
      ;; compute color
      (loop for c1 across guess
            for c2 across actual-word
            for i from 0
            for color = (serapeum:cond-let p
                          ((eql c1 c2)
                           2)
                          ((match-position c1)
                           (setf (aref matches p) 1)
                           1)
                          (t 0))
            do (incf total (* color (expt 3 i))))
      total)))

;; Memorize Colors
;; run (fill-color-table) before using simulate-game, entropy, or highest-entropy functions
(defparameter *color-table* (make-array (list (length *words*) (length *words*))
                                        :element-type 'color
                                        :initial-element 0))
(declaim (type (simple-array color) *color-table*))

(defun fill-color-table ()
  (lparallel:pdotimes (guess (1+ +max-index+))
    (loop for correct-word from 0 to +max-answer-word-index+ do
      (setf (aref *color-table* guess correct-word) (color0 (index->word guess)
                                                            (index->word correct-word))))))

(declaim (inline color))
(defun color (guess actual-word)
  (declare (optimize (speed 3))
           (type index guess actual-word))
  (the color (aref *color-table* guess actual-word)))

;;; Finding Best Guess
(declaim (ftype (function ((cons index color)) (vector index)) valid-guesses))
(defun valid-guesses (previous-guesses-and-colors)
  "return the `words' which are possible answers given the previous guesses and their color
`previous-guesses-and-colors' is a list of pairs (`guess-word' and `color')"
  (declare (optimize (speed 3) (space 3) (safety 0)))
  (let ((guesses (make-array 0 :element-type 'index :fill-pointer 0 :adjustable t)))
    (loop for i of-type index from 0 to +max-answer-word-index+
          when (every #'(lambda (pair)
                          (destructuring-bind (guess-word . color) pair
                            (declare (type color color))
                            (= (color guess-word i) color)))
                      previous-guesses-and-colors)
            do (vector-push-extend i guesses))
    guesses))

(defun entropy (word possible-words)
  "compute the average information that the `word' would reveal under given `possible-word'"
  (declare (optimize (speed 3) (safety 0))
           (type (array index) possible-words)
           (type index word))
  (let ((possibilites (make-array #.(expt 3 5) :element-type 'fixnum :initial-element 0)))
    (map 'nil #'(lambda (w)
                  (declare (type index w))
                  (incf (aref possibilites (color word w))))
         possible-words)
    (loop for k fixnum across possibilites
          with l fixnum = (length possible-words)
          for kbyl = (/ (coerce k 'single-float) l)
          unless (= k 0)
            summing (* kbyl (coerce (log kbyl) 'single-float)) into total single-float
          finally (return (* -1 total (/ (log 2)))))))

(defun highest-entropy (previous-guesses-and-colors)
  "find the `word' that reveals the highest average information given the previous guesses"
  (declare (optimize (speed 3) (safety 0))
           (type cons previous-guesses-and-colors))
  (let ((guesses (valid-guesses previous-guesses-and-colors)))
    (declare (type (array index) guesses))
    (if (<= (length guesses) 2)
        (aref guesses 0)
        (loop for i of-type fixnum from 0 to +max-index+
              for entropy single-float = (entropy i guesses)
              with max single-float = -1.0
              with maxi of-type index = 0
              do (when (> entropy max)
                   (setf max entropy
                         maxi i))
              finally (return maxi)))))


;; Simulating a game
(defun simulate-game (word)
  (declare (type index word)
           (optimize (speed 3) (safety 0) (debug 0)))
  (let ((previous-guesses ()))
    (loop repeat 6
          for guess = (index "trace")
            then (highest-entropy previous-guesses)
          for color = (color guess word) do
            (push (cons guess color) previous-guesses)
            (when (= guess word)
              (return)))
    (mapcar (alexandria:compose #'index->word #'car) previous-guesses)))

;;; Helpers
(defun encode-color (string)
  (loop for char across string
        for i from 0
        summing (* (alexandria:switch (char)
                     (#\b 0)
                     (#\y 1)
                     (#\g 2)
                     (t (error "invalid character; should be one of b,y,g")))
                   (expt 3 i))))

(defun decode-color (color)
  (let ((str (make-string 5 :initial-element #\b)))
    (loop for n = color then (truncate n 3)
          for i from 0
          when (= n 0)
            return str
          do (setf (char str i) (case (mod n 3)
                                  (0 #\b)
                                  (1 #\y)
                                  (2 #\g))))))

(defun encode (&rest previous-guesses-and-colors*)
  (loop for (guess color . rest) on previous-guesses-and-colors* by #'cddr
        collect (cons (index guess) (encode-color color))))

;; Benchmarking
;; before running benchmark run (fill-color-table)
(defparameter *games* (make-array (length *answers*)))
(defun run-all-games ()
  (loop for i from 0 to +max-answer-word-index+ do
    (setf (aref *games* i) (simulate-game i))
    (when (= 0 (mod i (* 23 5)))
      (format t "~&~,2f% Complete" (* (/ i +max-answer-word-index+) 100)))))

(defun prun-all-games ()
  "Parallely run all games"
  (lparallel:pdotimes (i (1+ +max-answer-word-index+))
    (setf (aref *games* i) (simulate-game i))))

(defun average-score (&optional (games *games*))
  (let ((steps 0)
        (pass 0)
        (failed 0))
    (loop for i from 0 to +max-answer-word-index+
          for key = (index->word i)
          for guesses in games do
            (if (string-equal key (first guesses))
                (setf pass (1+ pass)
                      steps (+ steps (length guesses)))
                (incf failed)))
    (values (coerce (/ steps pass) 'float)
            steps pass failed)))

(defun histogram (games)
  (let ((steps)
        (total (length games)))
    (loop for i from 0 to +max-answer-word-index+
          for key = (index->word i)
          for guesses across games do
            (if (string-equal key (first guesses))
                (incf (getf steps (length guesses) 0))
                (incf (getf steps :fail 0))))
    (loop for i from 1 to 6
          for percent = (* 100 (/ (or (getf steps i) 0) total)) do
            (format t "~&~a : ~4,1f%~%" i percent))
    (format t "~&>6: ~4d~%" (or (getf steps :fail) 0))))
