(in-package :cl-callbreak-bot)

(defconstant +spade+ 3)
(defconstant +highest-card-number+ 14)
(defconstant +lowest-card-number+ 2)

(defstruct card
  (color 0 :type (integer 0 3))
  (number 2 :type (integer 2 14)))

(declaim (inline card-= spade-card-p card-index card-deindex))
(defun card-= (card1 card2)
  (and (= (card-number card1)
          (card-number card2))
       (= (card-color card1)
          (card-color card2))))

(defun parse-card (card)
  (make-card
   :number (case (char card 0)
             (#\T 10)
             (#\J 11)
             (#\Q 12)
             (#\K 13)
             (#\1 14)
             (t (- (char-code (char card 0)) #.(char-code #\0))))
   :color (ecase (char card 1)
            (#\H 0)
            (#\C 1)
            (#\D 2)
            (#\S 3))))

(defun spade-card-p (card)
  (= (card-color card) +spade+))

(deftype card-index ()
  `(integer 0 51))

(defun card-index (card)
  (+ (* (card-color card) 13)
     (- (card-number card) +lowest-card-number+)))


(defun card-deindex (index)
  (let ((color (floor index 13)))
    (make-card :color color
               :number (+ +lowest-card-number+ (- index (* color 13))))))

(let ((card-deindex% (make-array 52 :element-type 'card
                                    :initial-contents (loop for i from 0 below 52
                                                            collect (card-deindex i)))))
  (defun card-deindex% (index)
    (aref card-deindex% index))

  (defun make-card* (&key color number)
    (aref card-deindex% (+ (- number +lowest-card-number+) (* color 13)))))

(defun encode-card (card)
  (let ((number (case (card-number card)
                  (10 #\T)
                  (11 #\J)
                  (12 #\Q)
                  (13 #\K)
                  (14 #\1)
                  (t (code-char (+ (card-number card) #.(char-code #\0))))))
        (color (case (card-color card)
                 (0 #\H)
                 (1 #\C)
                 (2 #\D)
                 (3 #\S))))
    (format nil "~c~c" number color)))

(defun encode-card-pretty (card)
  (let ((number (case (card-number card)
                  (10 #\T)
                  (11 #\J)
                  (12 #\Q)
                  (13 #\K)
                  (14 #\A)
                  (t (code-char (+ (card-number card) #.(char-code #\0))))))
        (color (case (card-color card)
                 (0 #\♥)
                 (1 #\♣)
                 (2 #\♦)
                 (3 #\♠))))
    (format nil "~c~c" number color)))

(defstruct player
  (id 0 :type player-id)
  (name "" :type string)
  (score 0e0 :type single-float)
  (bid 0 :type (integer 0 13))
  (won 0 :type (integer 0 13))
  (me? nil :type boolean)
  (cards #() :type (simple-array card 1))
  (card-prob (make-array (* 13 4) :initial-element 0.0e0 :element-type 'single-float)
   :type (simple-array single-float (52)))
  (data))

(defstruct history
  (first 0 :type player-id)
  (winner 0 :type player-id)
  (moves (make-array 4 :initial-element (make-card) :element-type 'card) :type (simple-array card (4))))

(defun history-= (h1 h2)
  (and (= (history-first h1) (history-first h2))
       (= (history-winner h1) (history-winner h2))
       (every #'card-= (history-moves h1) (history-moves h2))))

(defstruct game
  (players nil :type (simple-array player (4)))
  (me nil :type player)
  (round 1 :type (integer 1 5))
  (played-cards nil :type (simple-array card 1))
  (history (make-array 0 :element-type 'history) :type (simple-array history 1))
  (time-remaining 0e0 :type single-float))

(deftype dt ()
  `(simple-array single-float 1))
