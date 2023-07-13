(in-package :cl-callbreak-bot)

(u:eval-always
  (defvar *debugging* nil)
  (defmacro d! (&body body)
    "do things only when in debug mode"
    (when *debugging*
      `(progn
         ,@body)))

  (defvar *speed* 3)
  (defvar *debug* 0)
  (defvar *safety* 0)

  (defun declare-debug()
    (setf *debugging* t
          *speed* 0
          *debug* 3
          *safety* 3))

  (defun declare-speed()
    (setf *debugging* nil
          *speed* 3
          *debug* 0
          *safety* 0)))

(defun %map-random-cards2 (n function cards this-many dts selected-cards-vector)
  "randomly distribute considering proababilites of cards"
  (declare (optimize (speed #.*speed*) (debug #.*debug*) (safety #.*safety*))
           (type list cards this-many)
           (type fixnum n)
           (type function function)
           (type (simple-array (array card-index 1) 1) selected-cards-vector)
           (type list dts))
  (let* ((count 0)
           (nplayers (length cards))
           (total-cards (reduce #'+ this-many))
           (cards-to-use (make-array total-cards :initial-element -1 :element-type '(or (integer -1 -1) card-index)))
           (card-belongs (make-array total-cards :element-type '(simple-array single-float (4)))))
      (declare (type fixnum count))
      (assert (= 4 nplayers))
      (labels ((choose-player (belongs cards-required-count)
                 (let* ((choice (loop for belong single-float across (the (simple-array single-float (4)) belongs)
                                      for required-count fixnum across cards-required-count
                                      when (and (not (= belong 0.0))
                                                (not (= required-count 0)))
                                        summing belong into c single-float
                                      finally (return c))))
                   (declare (type single-float choice))
                   (when (= choice 0.0)
                     (throw 'backtrack nil))
                   (let* ((playeri (random choice))
                          (player (or (loop for belong across belongs
                                            for p fixnum from 0
                                            for req across cards-required-count
                                            when (and (not (= 0 req)) (>= belong playeri))
                                              return p
                                            when (and (not (= 0.0 belong)) (not (= req 0)))
                                              do (decf playeri belong))
                                      (error "not possible"))))
                     player)))

               (random-cards ()
                 (let ((cards-required-count (make-array nplayers :initial-contents this-many
                                                                  :element-type 'fixnum)))

                   (loop for s across selected-cards-vector
                         do (setf (fill-pointer s) 0))

                   (loop for card of-type card-index across cards-to-use
                         for belongs of-type (vector single-float 4) across card-belongs
                         do (let ((player (choose-player belongs cards-required-count)))
                              (declare (type fixnum player))
                              ;; make `card' to be used for player `player'
                              (vector-push card (aref selected-cards-vector player))
                              (decf (aref cards-required-count player)))))))

        (loop for cards* in cards
              for player fixnum from 0
              with i fixnum = 0
              do (loop for card in cards*
                       for pos = (position card cards-to-use) do
                         (unless pos
                           (setf (aref cards-to-use i) card)
                           (setf (aref card-belongs i) (make-array nplayers :element-type 'single-float))
                           (setf pos i)
                           (incf i))
                         (setf (aref (the (simple-array single-float (4)) (aref card-belongs pos)) player)
                               (aref (the dt (nth player dts)) card))))

        (loop named outer-loop do
          (loop repeat n
                do
                   (catch 'backtrack
                     (random-cards)

                     (funcall function selected-cards-vector)
                     (incf count)))
          (cond ((and (= count 0) (not (= n 0)))
                 ;; tried but didn't find even a single way
                 ;; proabably this is a bug at some other part of code
                 (print "WARNING: PROBABLE BUG!!")
                 (if (<= n 1)
                     (return-from outer-loop)))
                ((< count n)
                 (setf n (- n count)
                       count 0))
                (t
                 (return-from outer-loop)))))))

(defun generate-random-games-1 (n function game)
  (declare (optimize (speed #.*speed*) (debug #.*debug*) (safety #.*safety*))
           (type function function))
  (u:let+ ((dts (loop for player across (game-players game)
                      collect (player-card-prob player)))
           (played-cards-counts (loop for player across (game-players game)
                                      collect (length (the list (getf (player-data player) :played-cards)))))
           ((u:&values unsure-cards this-many) (unsure-cards-and-number dts played-cards-counts))
           (sure-cards (loop for dt of-type dt in dts
                             collect (loop for card from 0 below 52
                                           when (= (aref dt card) 1.0e0)
                                             collect card)))
           (selected-cards-vector (make-array 4 :initial-contents
                                              (loop for cc of-type fixnum in played-cards-counts
                                                    collect (make-array (- 13 cc)
                                                                        :element-type 'card-index
                                                                        :fill-pointer 0
                                                                        :adjustable nil))
                                              :element-type '(array card-index 1))))
    (%map-random-cards2 n (lambda (selected-cards-vector)
                            (let ((prob 1.0e0))
                              (declare (type single-float prob))
                              (loop for cards1 of-type (array card-index) across (the (simple-array t) selected-cards-vector)
                                    for dt of-type dt in dts
                                    for cards2 in sure-cards
                                    do
                                       (loop for card across cards1
                                             do (setf prob (* prob (aref dt card))))
                                       (loop for card of-type card-index in cards2
                                             do (vector-push card cards1)))
                              (funcall function selected-cards-vector prob)))
                      unsure-cards this-many dts selected-cards-vector)))
