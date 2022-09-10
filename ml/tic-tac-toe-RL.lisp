;;;; tic-tac-toe.lisp
(ql:quickload '(#:alexandria #:serapeum))
(uiop:define-package #:tic-tac-toe/utils
    (:use #:alexandria #:serapeum)
  (:reexport #:alexandria #:serapeum))

(defpackage #:tic-tac-toe
  (:use #:cl)
  (:local-nicknames (#:u #:tic-tac-toe/utils)))

(in-package #:tic-tac-toe)
(declaim (optimize (safety 0) (speed 3) (debug 0)))
;;;; Utilities

;;; Board

(deftype board ()
  `(simple-array (integer 0 2) (9)))

(deftype board-number ()
  `(integer 0 ,(1- (expt 3 9))))

(deftype player ()
  `(integer 1 2))

(defun make-board ()
  (make-array 9 :element-type '(integer 0 2)))

(defun board-number (board)
  (declare (type board board))
  (let ((powers-of-three #.(map 'vector (lambda (k) (expt 3 k)) (u:iota 9))))
    (loop for x across board
          for y across powers-of-three
          for k from 0
          summing (* x y))))

(defun number-board (number)
  (declare (board-number number))
  (let ((board (make-board)))
    (do ((n number (truncate n 3))
         (i 0 (1+ i)))
        ((= n 0) board)
      (setf (aref board i) (mod n 3)))))

(defconstant +winning-positions+
  '((0 1 2) (3 4 5) (6 7 8)
    (0 3 6) (1 4 7) (2 5 8)
    (0 4 8) (2 4 6)))

(defun winner (board)
  (declare (type board board))
  (flet ((all-same (pos)
           (loop for p in (rest pos)
                 with k = (aref board (car pos))
                 when (or (= k 0)
                          (not (= k (aref board p))))
                   return nil
                 finally (return k))))
  (loop for pos in +winning-positions+
        for same = (all-same pos)
        when same
          do (return same))))

;;; Moves

(defun make-move (player board pos)
  (declare (type player player)
          (type (integer 0 8) pos)
          (type board board))
  (setf (aref board pos) player))

(defun make-move-n (player board-number pos)
  (declare (type player player)
           (type (integer 0 8) pos)
           (type board-number board-number))
  (+ board-number (* player (expt 3 pos))))

(defun valid-moves (board)
  (loop for v across board
        for k from 0
        when (= v 0)
          collect k))

;;; Value Table

(defun transpose (board map)
  (map (if (listp board)
           'list
           'board)
       (lambda (pos)
         (elt board pos))
       map))

(defparameter *value-table* (make-array (expt 3 9)
                                        :element-type 'single-float
                                        :initial-element 0.0))
(defparameter *inverted-board-numbers* (make-array (expt 3 9)
                                        :element-type 'fixnum
                                        :initial-element 0))
(defparameter *inverted-symmetry-normalized-board-numbers* (make-array (expt 3 9)
                                        :element-type 'fixnum
                                        :initial-element 0))

(defparameter *symmetry-normalized-board-numbers*
  (make-array (expt 3 9)
              :element-type 'fixnum
              :initial-element 0))

(defparameter *symmetries0*
  '((2 1 0 5 4 3 8 7 6) ;; Vertical Flip
    (6 7 8 3 4 5 0 1 2) ;; Horizontal Flip
    (0 3 6 1 4 7 2 5 8) ;; Main Diagonal Flip
    (8 5 2 7 4 1 6 3 0))) ;; Off Diagonal Flip

(defparameter *symmetries* ;; above symmteries and their combinations
  (let ((res nil))
    (u:map-combinations (lambda (syms)
                          (push (reduce #'transpose syms :initial-value '(0 1 2 3 4 5 6 7 8))
                                res))
                        *symmetries0*
                        :length 2)
    (concatenate 'list *symmetries0* res)))

(defun symmetric? (b1 b2)
  (flet ((sym? (sym)
           (every (lambda (p)
                    (= (aref b1 p) (aref b2 p)))
                  sym)))
    (some #'sym? *symmetries*)))

(defun value (bn player)
  (case player
    (2 (aref *value-table* (aref *symmetry-normalized-board-numbers* bn)))
    (1 (aref *value-table* (aref *inverted-symmetry-normalized-board-numbers* bn)))))

(defun set-value (bn player value)
  (case player
    (2 (setf (aref *value-table* (aref *symmetry-normalized-board-numbers* bn)) value))
    (1 (setf (aref *value-table* (aref *inverted-symmetry-normalized-board-numbers* bn)) value))))

(defun symmetries-n (board)
  (map 'vector (lambda (sym)
                 (board-number (transpose board sym)))
       *symmetries*))

(defun invert-board (board)
  (map 'board (lambda (k)
                (case k
                  (0 0)
                  (1 2)
                  (2 1)))
       board))

(defun initialize-value-table ()
  (setf *value-table* (make-array (expt 3 9)
                                  :element-type 'single-float
                                  :initial-element 0.0))
  (loop for x from 0 below (expt 3 9)
        for board = (number-board x)
        for base = (u:extremum (symmetries-n board) #'<)
        do (setf (aref *symmetry-normalized-board-numbers* x) base))

  (loop for x from 0 below (expt 3 9) do
    (setf (aref *inverted-board-numbers* x) (board-number (invert-board (number-board x)))))

  (loop for x from 0 below (expt 3 9) do
    (setf (aref *inverted-symmetry-normalized-board-numbers* x)
          (aref *symmetry-normalized-board-numbers* (aref *inverted-board-numbers* x))))
  (loop for x from 0 below (expt 3 9)
        for winner = (winner (number-board x))
        do (setf (aref *value-table* x)
                 (ecase winner
                   (1 0.0)
                   (2 1.0)
                   ((nil) 0.5)))))
;;;; Learning

;;; Players

(defun greedy-player (board player)
  (declare (type board board)
           (type player player))
  (let ((board-n (board-number board)))
    (flet ((move-value (move)
             (value (make-move-n player board-n move) player)))
      (u:extremum (valid-moves board)
                  #'> :key #'move-value))))

(defparameter *alternative-vt* (let ((*value-table* nil))
                                 (initialize-value-table)
                                 *value-table*))
(defun alternative-greedy-player (board player)
  (declare (type board board)
           (type player player))
  (let ((*value-table* *alternative-vt*))
    (greedy-player board player)))

(defun epsilon-greedy-player (board player &optional (epsilon 0.1))
  (declare (type board board)
           (type player player))
  (if (< (random 1.0) epsilon)
      (u:random-elt (valid-moves board))
      (greedy-player board player)))

(defun random-player (board player)
  (declare (type board board)
           (ignore player))
  (u:random-elt (valid-moves board)))

(defmacro d! (&body body)
  (declare (ignorable body))
  `())

;;; Play and Learn
(defun play-and-learn (&optional (alpha 0.1) (opponent #'random-player))
  (let ((board (make-board))
        (boards nil))
    (loop repeat 9
          for player = (1+ (random 2)) then (1+ (mod player 2))
          do
             (d! (render-board board))
             (ecase player
               ;; opponent
               (1 (let ((move (funcall opponent board 1)))
                    (make-move 1 board move)))
               (2 (let* ((move (epsilon-greedy-player board player 0.5))
                         (bn+1 (make-move-n 2 (board-number board) move)))
                    (make-move 2 board move)
                    (push bn+1 boards))))
             (when (winner board)
               (return)))
    ;; update value table
    (d! (render-board board))
    (loop for p in boards
          with vt+1 = (value (board-number board) 2)
          for vt = (value p 2) do
            (d! (print vt+1))
            (setf vt+1
                  (set-value p 2
                             (+ vt (* alpha (- vt+1 vt)))))
          finally (d! (print vt+1)))))


;;;; UI

(defun render-board (board)
  (format t "~&")
  (loop for row from 0 below 3
        with i = 0 do
          (format t "~%")
          (loop for col from 0 below 3 do
                 (format t "~d | " (ecase (aref board i)
                                     (0 " ")
                                     (1 "O")
                                     (2 "X")))
                 (incf i))))

(defun simulation (player-1 player-2 render-function)
  (let ((board (make-board)))
    (prog1 (loop repeat 9
                 for player = (1+ (random 2)) then (1+ (mod player 2)) do
                   (funcall render-function board)
                   (ecase player
                     (1
                      (make-move 1 board (funcall player-1 board 1)))
                     (2
                      (make-move 2 board (funcall player-2 board 2))))
                   (u:when-let (winner (winner board))
                     (return winner)))
      (funcall render-function board))))

(defun test-winrate (&optional (n 10000) (opponent #'random-player))
  (let ((wins 0)
        (draws 0))
    (loop for winner = (simulation opponent #'greedy-player #'identity)
          repeat n do
            (case winner
              (2 (incf wins))
              ((nil) (incf draws))))
    (format t "~&Wins: ~,3f~%Draws: ~,3f~%" (/ wins n) (/ draws n))))

(defun play-with-human ()
  (let ((winner (simulation
                 (lambda (board player)
                   (loop for m in (valid-moves board) do
                     (print (cons m (value (make-move-n 1 (board-number board) m) player))))
                   (format t "~&Your Move: ")
                   (read))
                 (lambda (board player)
                   (loop for m in (valid-moves board) do
                     (print (cons m (value (make-move-n 2 (board-number board) m) player))))
                   (greedy-player board player))
                 (lambda (board)
                   (format t "~%~%")
                   (render-board board)))))
    (format t "~&Winner is: ~a" (ecase winner
                                  (1 "You")
                                  (2 "Computer")
                                  ((nil) "Draw")))))


;;; Train

(defun train (&optional (max most-positive-fixnum))
  (loop for i from 1 below max do 
    (play-and-learn 0.1 #'epsilon-greedy-player)))

#+test
(let ((vt *value-table*)
      (avt *alternative-vt*))
  (macrolet ((wrap-with-vt (vt player-func)
               `(lambda (board player)
                  (let ((*value-table* ,vt))
                    (,player-func board player)))))
    (time (loop for i from 0 do
      (when (= 0 (mod i (expt 10 5)))
        (format t "~&Traning Iteration: ~:d" i)
        (when (= 0 (mod i (expt 10 7)))
          (test-winrate 100000)))
      (play-and-learn 0.1 (wrap-with-vt avt greedy-player))
      (let ((*value-table* *alternative-vt*))
        (play-and-learn 0.1 (wrap-with-vt vt greedy-player)))))))


;;; Results
#||
TIC-TAC-TOE> (initialize-value-table)

TIC-TAC-TOE> (time (train 1000000))
Evaluation took:
  4.856 seconds of real time
  4.861192 seconds of total run time (4.703605 user, 0.157587 system)
  [ Run times consist of 0.042 seconds GC time, and 4.820 seconds non-GC time. ]
  100.10% CPU
  919,225,840 bytes consed

TIC-TAC-TOE> (test-winrate 10000 #'random-player)
Wins: 0.947
Draws: 0.053

TIC-TAC-TOE> (test-winrate 10000 #'greedy-player)

Wins: 0.000
Draws: 1.000 
||#
