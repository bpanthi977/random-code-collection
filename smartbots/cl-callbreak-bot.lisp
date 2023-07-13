;;;; cl-callbreak-bot.lisp
(in-package #:cl-callbreak-bot)

;; Data Types & Parsers

(defparameter *print-inference* nil)
(defparameter *got-request-at* nil)
(defparameter *overhead-data* nil)
(declaim (type fixnum *time-taken-for-thinking*))
(defparameter *time-taken-for-thinking* 0)
(defparameter *total-iterations* 1)
(defparameter *total-time* 1)
(defparameter *mcts-overhead* 0.0e0)
(defparameter *mcts-overhead-runs* 0)

(declaim (type single-float *avg-overhead*))
(defparameter *avg-overhead* 10.0e0)
(defparameter *overhead-count* 0)


(declaim (type (simple-array fixnum 1) *time-for-response*))
(declaim (type (simple-array single-float 1) *time-for-response-count*))
(defparameter *time-for-response* (make-array 14 :element-type 'fixnum :adjustable nil))
(defparameter *time-for-response-count* (make-array 14 :element-type 'single-float :adjustable nil))


(declaim (type (simple-array single-float 3) *card-distribution-table*))
(defparameter *card-distribution-table* #.(u:map-array
                                           (lambda (i) (coerce i 'single-float))
                                           (uiop:read-file-form
                                            (asdf:system-relative-pathname :cl-callbreak-bot
                                                                           "./card-distribution-table-real-word.data"))
                                           :element-type 'single-float
                                           :fill-pointer nil
                                           :adjustable nil))

(defmacro measuring-time ((ncards time-factor) &body body)
  (u:once-only (ncards)
    (u:with-gensyms (t0 result)
      `(let ((,t0 (get-internal-real-time))
             (,result (multiple-value-list
                       (progn
                         ,@body))))
         (setf *time-taken-for-thinking* (- (get-internal-real-time) ,t0))
         (incf (aref *time-for-response* ,ncards) *time-taken-for-thinking*)
         (incf (aref *time-for-response-count* ,ncards) (coerce ,time-factor 'single-float))
         (apply #'values ,result)))))

(defun parse-history (entry)
  (declare (optimize (speed #.*speed*) (debug #.*debug*) (safety #.*safety*)))
  (make-history :first (first entry)
                :winner (third entry)
                :moves (map '(simple-array card 1) #'parse-card (second entry))))

(defun parse-json-new (text)
  (declare (optimize (speed #.*speed*) (debug #.*debug*) (safety #.*safety*)))
  (let* ((json (yason:parse text))
         (me (gethash "playerId" json))
         (players (loop for name in (gethash "playerIds" json)
                        for context = (u:href json "context" "players" name)
                        for me? = (string-equal name me)
                        for id from 0
                        collect (make-player :id id
                                             :name name
                                             :score (coerce (gethash "totalPoints" context) 'single-float)
                                             :bid (gethash "bid" context)
                                             :won (gethash "won" context)
                                             :me? me?
                                             :cards (if me?
                                                        (map '(simple-array card 1)
                                                             #'parse-card
                                                             (gethash "cards" json))
                                                        #())
                                             :data (if me?
                                                       (list :played-cards nil)
                                                       (list :played-cards nil))))))
    (make-game :round (u:href json "context" "round")
               :players (u:map-list 'player (make-player) #'identity players)
               :me (find t players :key #'player-me?)
               :played-cards (map '(simple-array card 1) #'parse-card (gethash "played" json nil))
               :history (map '(simple-array history 1) #'parse-history (gethash "history" json nil))
               :time-remaining (coerce (gethash "timeBudget" json 1500) 'single-float))))

(defun whole-game-first-player-id (game)
  (declare (optimize (speed #.*speed*) (debug #.*debug*) (safety #.*safety*)))
  (if (not (= 0 (length (game-history game))))
      (history-first (aref (game-history game) 0))
      (mod (- (player-id (game-me game))
              (length (game-played-cards game)))
           4)))

(defun card-distribution-probs (bid turn)
  (declare (optimize (speed #.*speed*) (debug #.*debug*) (safety #.*safety*))
           (type fixnum bid turn))
  (let ((df (make-array 52 :initial-element 0.0e0 :element-type 'single-float))
        (bid (- (u:clamp bid 1 8) 1)))
    (assert (<= 0 turn 3))
    (loop for i from 0 below 52 do
      (setf (aref df i) (coerce (aref *card-distribution-table* bid turn i) 'single-float)))
    df))

(defun add-prior-probabilities (new-game)
  (declare (optimize (speed #.*speed*) (debug #.*debug*) (safety #.*safety*)))
  (loop for player across (game-players new-game)
        do
           (cond ((eql player (game-me new-game))
                  (setf (player-card-prob player) (make-array (* 13 4) :initial-element 0.0e0 :element-type 'single-float)))
                 ((= (player-bid player) 0)
                  (setf (player-card-prob player) (make-array (* 13 4) :initial-element 0.33333e0 :element-type 'single-float)))
                 (t
                  (setf (player-card-prob player)
                        (card-distribution-probs (player-bid player)
                                                 (mod (- (player-id player) (the fixnum (whole-game-first-player-id new-game)))
                                                      4))))))
  (loop for card across (player-cards (game-me new-game))
        do
           (loop for player across (game-players new-game)
                 do (setf (probability player card) 0))
           (setf (probability (game-me new-game) card) 1))

  ;; normalize
  (let ((sum (make-array 52 :initial-element 0.0e0 :element-type 'single-float)))
    (loop for player across (game-players new-game)
          do
             (loop for prob single-float across (the dt (player-card-prob player))
                   for i fixnum from 0 do
                     (incf (aref sum i) prob)))
    (loop for player across (game-players new-game)
          for dt of-type dt = (player-card-prob player)
          do
             (loop for i from 0 below 52 do
               (setf (aref dt i) (/ (aref dt i) (aref sum i))))))
  new-game)


(defun parse-json (text)
  (let ((game (add-prior-probabilities (parse-json-new text))))
    game))


;; Helpers
(declaim (inline my-cards highest-card higher-cards remove-card same-color-cards))

(defun my-cards (game)
  (player-cards (game-me game)))

(defun highest-card (cards color)
  (u:extrema (u:keep color cards :key #'card-color)
             #'> :key #'card-number))

(defun higher-cards (cards value)
  (remove-if (u:partial #'> value) cards :key #'card-number))

(defun remove-card (card cards player)
  (loop for cards* in cards
        for i from 0
        collect (if (= i player)
                    (remove card cards*)
                    cards*)))


(defun same-color-cards (cards color)
  (u:keep color cards :key #'card-color))

(defun wins-if-no-spades (cards1 cards2)
  "`cards1' and `cards2' are set of cards of same color
return in any worst case scenario how many cards from `cards1' win over `cards2'"
  (let ((cards1 (sort cards1 #'> :key #'card-number))
        (cards2 (sort cards2 #'> :key #'card-number))
        (win 0))
    (loop for card1 in cards1 do
      (cond ((null cards2)
             (incf win))
            ((> (card-number card1) (card-number (first cards2)))
             ;; no card in cards2 win over card1, so throw the lowest card in cards2
             (setf cards2 (butlast cards2))
             (incf win))
            (t
             ;; else card in cards2 wins over card1
             (setf cards2 (cdr cards2)))))
    win))


(defun valid-moves** (player-cards hcc hsc)
  (if (not hcc)
      ;; I am first player => can play anything
      (values player-cards t)
      (let* ((first-card-color (card-color hcc))
             (spades? (not (not hsc)))
             (same-color-cards (u:keep first-card-color player-cards :key #'card-color)))
        (cond
          ;; if you have same color card, you have to play one of the same color cards
          (same-color-cards
           ;; if spade has been played, you can play any same color card
           (if (and spades? (not (= first-card-color +spade+)))
               (values same-color-cards nil)
               ;; if not either play winning card from same color cards
               (let ((higher-cards (higher-cards same-color-cards (card-number hcc))))
                 (if higher-cards
                     (values higher-cards t)
                     ;; or any of the same color cards
                     (values same-color-cards nil)))))
          ;; if you have spades, then you can play one of the spades
          ((find +spade+ player-cards :key #'card-color)
           (let* ((spades (u:keep +spade+ player-cards :key #'card-color))
                  (winning-spades (if hsc
                                      (higher-cards spades (card-number hsc))
                                      spades)))
             ;; if you have higher spade, play one of them
             (if winning-spades
                 (values winning-spades t)
                 ;; else play any of the spades or other color
                 (values player-cards nil))))
          ;; and if you don't even have spades, you can play anything
          (t (values player-cards nil))))))

(defun hcc-hsc (played-cards)
  (if (not (= 0 (length played-cards)))
      (values (highest-card played-cards (card-color (elt played-cards 0)))
              (highest-card played-cards +spade+))
      (values nil nil)))

(defun valid-moves* (player-cards played-cards)
  (multiple-value-bind (hcc hsc) (hcc-hsc played-cards)
    (valid-moves** player-cards hcc hsc)))

(defun valid-moves (player played-cards)
  (valid-moves* (player-cards player) played-cards))

(defun highest-possible-card (playeri color game)
  (loop with cards = (player-card-prob (aref (game-players game) playeri))
        for index from (- (* (1+ color) 13) 1) downto (* color 13)
        when (not (= 0 (aref cards index)))
          return (+ +lowest-card-number+ (- index (* color 13)))))

(defun only-one (moves turn)
  (declare (optimize (speed #.*speed*) (debug #.*debug*) (safety #.*safety*))
           (type (simple-array card 1) moves))
  (cond ((= (length moves) 1)
         (elt moves 0))
        ((= turn 3)
         (u:extremum moves #'< :key #'card-number))
        (t
         (multiple-value-bind (min max) (u:extrema moves #'< :key #'card-number)
           (when (= (- (card-number max) (card-number min)) (1- (length moves)))
             ;; all cards are consecutive, throw any one of them
             (elt moves 0))))))

(defun consecutive-cards? (player-cards)
  (declare (optimize (speed #.*speed*) (debug #.*debug*) (safety #.*safety*))
           (type (simple-array card 1) player-cards))
  (let ((fcc (card-color (aref player-cards 0))))
    (and (every (lambda (card)
                  (= (card-color card) fcc))
                player-cards)
         (progn (sort player-cards #'> :key #'card-number)
                (loop for i from 1 below (length player-cards)
                      unless (= (1+ (card-number (aref player-cards i)))
                                (card-number (aref player-cards (- i 1))))
                        return nil
                      finally (return t))))))

(defun only-sensible-move (game)
  (declare (optimize (speed #.*speed*) (debug #.*debug*) (safety #.*safety*)))
  (u:let+ ((player (game-me game))
           (player-cards (player-cards player))
           (played-cards (game-played-cards game))
           (turn (length (game-played-cards game)))
           ((u:&values hcc hsc) (hcc-hsc played-cards)))
    (cond ((= (length player-cards) 1) (aref player-cards 0)) ;; only one card? play that
          ((consecutive-cards? player-cards) (aref player-cards 0)) ;; all cards are consecutive? play any one
          ((= (length played-cards) 0) nil) ;; first move? there is no trivially the-only-sensible move
          (t
           (let* ((first-card-color (card-color hcc))
                  (spades? (not (not hsc)))
                  (same-color-cards (u:keep first-card-color player-cards :key #'card-color)))
             (cond
               ;; if you have same color card, you have to play one of the same color cards
               ((not (= 0 (length same-color-cards)))
                ;; if spade has been played, you play the lowest same color card
                (if (and spades? (not (= first-card-color +spade+)))
                    (u:extremum same-color-cards #'< :key #'card-number)
                    ;; if not either play winning card from same color cards
                    (let ((higher-cards (higher-cards same-color-cards (card-number hcc))))
                      (if (not (= 0 (length higher-cards)))
                          ;; play the lowest card that can always win
                          (only-one higher-cards turn)
                          ;; or lowest of the same color cards
                          (u:extremum same-color-cards #'< :key #'card-number)))))
               ;; if you have spades, then you can play one of the spades
               ((find +spade+ player-cards :key #'card-color)
                (let* ((spades (u:keep +spade+ player-cards :key #'card-color))
                       (winning-spades (if hsc
                                           (higher-cards spades (card-number hsc))
                                           spades)))
                  ;; if you have higher spade, play one of them
                  (if (not (= 0 (length winning-spades)))
                      (only-one winning-spades turn)
                      nil)))
               ;; and if you don't even have spades, you can play anything
               (t nil)))))))

(defun range-of-cards (low high color)
  (loop for i from (max low +lowest-card-number+) to (min high +highest-card-number+)
        collect (make-card :color color
                           :number i)))

(defun all-cards (color)
  (range-of-cards +lowest-card-number+ +highest-card-number+ color))

(defun probability (player card)
  (aref (player-card-prob player) (card-index card)))

(defun (setf probability) (value player card)
  (setf (aref (player-card-prob player) (card-index card)) (coerce value 'single-float)))

(defun remove-card-from-game (game card)
  (loop for player across (game-players game)
        do (setf (probability player card) 0)))

(defun player-has (player card game probability)
  (cond ((= probability 1)
         (remove-card-from-game card game)
         (setf (probability player card) probability))
        (t
         (setf (probability player card) (* (probability player card) probability))
         ;; normalize other probabilities
         (let ((total (loop for player across (game-players game)
                            summing (probability player card))))
           (unless (= total 0)
             (loop for player across (game-players game)
                   do (setf (probability player card)
                            (/ (probability player card) total))))))))

(defun player-has-not (player cards game &key (probability 1))
  (mapcar (lambda (card)
            (player-has player card game (- 1 probability)))
          cards))

;;; probability table computation
(defun unsure-cards-and-number (dts played-cards-counts)
  (let* ((card-exists (make-array 52 :element-type 'bit))
         (unsure-cards (loop for dt in dts
                             for i from 0
                             collect (loop for card from 0 below 52
                                           when (and (not (= (aref dt card) 1))
                                                     (not (= (aref dt card) 0)))
                                             collect card
                                           do (when (not (= (aref dt card) 0))
                                                (setf (aref card-exists card) 1)))))
         (this-many (loop for dt in dts
                          for played-cards-count in played-cards-counts
                          collect (- 13 played-cards-count (count-if (u:partial #'= 1) dt)))))
    (values unsure-cards this-many)))

(defun wins? (card hcc hsc)
  (or
   ;; no card played till now
   (and (null hcc) (null hsc))
   ;; no spade played, and has higher same color
   (and (null hsc)
        (= (card-color card) (card-color hcc))
        (> (card-number card) (card-number hcc)))
   ;; higher spader
   (and (= (card-color card) +spade+)
        (or (null hsc)
            (> (card-number card) (card-number hsc))))))

(defun new-hcc-hsc (card hcc0 hsc0)
  "Returns the new highest color card, and highest spade card after `card' has been played"
  (values (if (or (null hcc0)
                  (and (= (card-color card) (card-color hcc0))
                       (> (card-number card) (card-number hcc0))))
              card
              hcc0)
          (if (not (= (card-color card) +spade+))
              hsc0
              (if (or (null hsc0)
                      (> (card-number card) (card-number hsc0)))
                  card hsc0))))


;; Deterministic Play Simulation
(defun generate-random-game (my-cards* myi)
  (declare (optimize (speed #.*speed*) (debug #.*debug*) (safety #.*safety*)))
  (d! (assert (= (length my-cards*) 13)))
  (let* ((used-cards (make-array 52 :element-type 'bit))
         (my-cards (map 'list #'card-index my-cards*))
         (all-cards (list my-cards* nil nil nil)))

    (loop for card in my-cards
          do (setf (aref used-cards card) 1))

    (loop repeat (- 52 13)
          for count fixnum from 13
          for new-card = (random (- 52 count))
          with player fixnum = 0
          do
             ;; deal the next ith card to player
             (when (= (mod count 13) 0)
               (incf player 1))
             (let ((card (loop for i from 0
                               with count fixnum = 0 do
                                 (when (= 0 (aref used-cards i))
                                   (if (= count new-card)
                                       (return i)
                                       (incf count))))))
               (push (card-deindex card) (nth player all-cards))
               (setf (aref used-cards card) 1)))
    (rotatef (nth 0 all-cards) (nth myi all-cards))
    all-cards))

(defun winning-card-exists? (cards card)
  "Does any card that wins `card' exists in `cards'"
  (or ;; higher cards of same color
   (find-if (lambda (c)
              (and (= (card-color c) (card-color card))
                   (> (card-number c) (card-number card))))
            cards)
   ;; no card of same color, but a spade
   (when (and (not (= (card-color card) +spade+))
              (not (find (card-color card) cards :key #'card-color)))
     (find +spade+ cards :key #'card-color))))

(defun winner-changes? (cards highest-card turns)
  (cond ((null turns) nil) ;; no one left to change winning card
        (t
         (let ((next-player-cards (nth (car turns) cards)))
           (or (winning-card-exists? next-player-cards highest-card)
               (winner-changes? cards highest-card (rest turns)))))))

(defun winning-cards (cards valid-moves remaining-turns hcc hsc)
  (let ((winning-cards (remove-if-not (u:rcurry #'wins? hcc hsc) valid-moves)))
    (if (null remaining-turns)
        winning-cards
        (loop for card in winning-cards
              unless (multiple-value-bind (hcc hsc) (new-hcc-hsc card hcc hsc)
                       (winner-changes? cards (or hsc hcc) remaining-turns))
                collect card))))

(defun lowest-card (valid-moves)
  "return the lowest card preferably not a spade"
  (loop for card in valid-moves
        with low-spade = nil
        with low-card = nil
        do (if (= (card-color card) +spade+)
               (when (or (not low-spade)
                         (< (card-number card) (card-number low-spade)))
                 (setf low-spade card))
               (when (or (not low-card)
                         (< (card-number card) (card-number low-card)))
                 (setf low-card card)))
        finally (return (or low-card low-spade))))

(defun play-move-v2 (cards hcc hsc turns)
  "returns a card preferably that wins the game"
  (declare (optimize (speed #.*speed*) (debug #.*debug*) (safety #.*safety*)))
  (let* ((player-cards (nth (first turns) cards))
         (winning-cards nil))
    (multiple-value-bind (valid-moves wins?) (valid-moves** player-cards hcc hsc)
      (when wins?
        (if (not (rest turns))
            (setf winning-cards valid-moves)
            (setf winning-cards
                  (loop for card in valid-moves
                        unless (multiple-value-bind (hcc hsc) (new-hcc-hsc card hcc hsc)
                               (winner-changes? cards (or hsc hcc) (cdr turns)))
                          collect card))))
      (cond ((or (not wins?) (null winning-cards))
             ;; if no winning card, then throw the lowest card of suit with least number of cards
             ;; and preferably not a spade
             (let ((spade-count (count 3 valid-moves :key #'card-color)))
               (if (= spade-count 0)
                   (values (lowest-card valid-moves) nil)
                   (let* ((suits-card-count (loop for color from 0 to 2
                                                  collect (cons color (count color valid-moves :key #'card-color))))
                          (sorted-count (sort suits-card-count #'< :key #'cdr))
                          ;; first suit with non zero card count
                          (selected (find-if (lambda (count)
                                               (> count 0))
                                             sorted-count :key #'cdr)))
                     (if (not selected)
                         ;; if no other card than spade, throw lowest spade card
                         (values (u:extremum valid-moves #'< :key #'card-number) nil)
                         ;; else throw lowest card of that color
                         (values (u:extremum (remove-if-not (u:partial #'= (car selected)) valid-moves :key #'card-color)
                                             #'< :key #'card-number)
                                 nil))))))
            ;; else one of the winning cards
            ;; if non-spades exists one of the non-spades, else
            ;; the lowest spade
            (t (values (if (find +spade+ winning-cards :key #'card-color :test-not #'=)
                           (u:random-elt (remove +spade+ winning-cards :key #'card-color))
                           (u:extrema winning-cards #'< :key #'card-number))
                       t))))))

(defun assort-and-sort (cards)
  "group by color and then sort by card-number"
  (mapcar (lambda (cards)
            (sort cards #'> :key #'card-number))
          (u:assort cards :key #'card-color)))

(defun higher-cards-count (cards color number)
  (count-if (lambda (card)
              (and (= (card-color card) color)
                   (> (card-number card) number)))
            cards))

(defun play-opening-move-v4 (cards turns)
  "returns a card preferably that wins the game"
  (let* ((player-cards (nth (first turns) cards))
         (others-cards (remove (nth (first turns) cards) cards))
         (valid-moves (valid-moves** player-cards nil nil))
         (winning-cards (winning-cards cards valid-moves (rest turns) nil nil))
         (all-my-cards-sorted (assort-and-sort valid-moves))
         (my-spades-sorted (find-if #'spade-card-p all-my-cards-sorted :key #'first))
         (my-color-cards-sorted (remove-if #'spade-card-p all-my-cards-sorted :key #'first))
         (rule-one-donot-throw-colors nil)
         (rule-two-points-and-spades nil)
         (rule-three-points-and-cards nil))
    ;; rule 1: wait to make lower card win
    (labels ((does-rule-one-apply (color p my-cards-sorted)
             ;; everyone has either at least `n' color cards or no spades
               (let ((n (u:extrema (mapcar (lambda (cards)
                                             (let ((same-color (count color cards :key #'card-color)))
                                               (if (not (= same-color 0))
                                                   same-color
                                                   (if (find +spade+ cards :key #'card-color)
                                                       0
                                                       14))))
                                           others-cards)
                                 #'<)))
               (assert (not (eql n 14))) ;; because there is at least one non winning cards in `my-cards'
               ;; and for that someone must have either a spade or a same color card

               ;; `n' is greater than the number of my winning cards `p'
               (when (> n p)
                 (let* ((largest-non-winning-card (nth p my-cards-sorted))
                        (number (card-number largest-non-winning-card)))
                   ;; others don'e have more than `p' cards greater than my `largest-non-winning-card'
                   (every (lambda (cards)
                            (<= (higher-cards-count cards color number) p))
                          others-cards)))))

           (apply-rule-one ()
             "don't throw winning higher cards to make lower cards win"
             (loop for sorted-cards in all-my-cards-sorted
                   for color = (card-color (first sorted-cards))
                   for p = (count color winning-cards :key #'card-color)
                   when (and (not (= 0 p)) ;; at least one winning card of that color
                             (> (length sorted-cards) p) ;; and at least one non winning cards of that color
                             (does-rule-one-apply color p sorted-cards))
                     do (push color rule-one-donot-throw-colors)))

           (count-spades (cards)
             (count #'spade-card-p cards))

           (winner-index (list-of-sorted-cards)
             (loop for i from 1
                   for cards in (rest list-of-sorted-cards)
                   for n = (if cards (card-number (first cards)) 0)
                   with winner = 0
                   with card = (card-number (first (first list-of-sorted-cards)))
                   do (when (> n card)
                        (setf winner i
                              card n))
                   finally (return winner)))

           (extra-points-and-spades-to-eliminate (my-cards others-cards spades-count)
             "all cards are sorted"
             (let ((points 0)
                   (spades 0)
                   (once-spade nil))
               (loop repeat (length my-cards)
                     for winner = (winner-index (cons my-cards others-cards))
                     do
                        (if (= winner 0)
                            ;; I win, but check if there are spades
                            (loop for cards in others-cards
                                  for spades in spades-count
                                  with some-null = nil
                                  with encountered-spades = nil
                                  do
                                  (when (null cards)
                                    (setf some-null t)
                                    (if (> spades 0)
                                        (setf encountered-spades t)))
                                  finally (when (or encountered-spades once-spade)
                                            (setf once-spade t)
                                            (incf points)
                                            (when encountered-spades (incf spades)))))
                        ;; remove cards from game
                        (loop for cards in others-cards
                              for spades in spades-count
                              for i from 1
                              collect (if (= i winner)
                                          (cdr cards)
                                          (butlast cards))
                                into other-cards*
                              collect (if (and (null cards) (> spades 0))
                                          (- spades 1)
                                          spades)
                                into spades-count*
                              finally (setf other-cards other-cards*
                                            spades-count spades-count*)))
               (values points spades)))

           (apply-rule-two ()
             "bring down other's spades so that my cards can win"
             (unless (= 0 (length my-spades-sorted))
               (loop named outer-loop
                     for sorted-cards in my-color-cards-sorted
                     for color = (card-color (first sorted-cards))
                     with p = (length my-spades-sorted)
                     for winning-cards-count = (count color winning-cards :key #'card-color) do
                       (let* ((spades-count (mapcar #'count-spades others-cards))
                              ;; max number of spades any other player has
                              (s (u:extremum spades-count #'>)))
                         (when (> s p)
                           (return-from outer-loop))
                         (multiple-value-bind (points spades)
                             (extra-points-and-spades-to-eliminate
                              sorted-cards
                              (mapcar (lambda (cards)
                                        (sort (same-color-cards cards color) #'> :key #'card-number))
                                      others-cards)
                               spades-count)
                           (when (> points 0)
                             (push (cons points spades) rule-two-points-and-spades)))))))

             (apply-rule-three ()
               "get rid of few cards to win over others cards by trumping"
               (loop for my-cards in my-color-cards-sorted
                     for color = (card-color (first my-cards))
                     for n = (length my-cards)
                     when (not (find color winning-cards :key #'card-color))
                       do
                          (let ((min (u:extrema (mapcar (lambda (cards)
                                                       (count-if (lambda (card)
                                                                   (= (card-color card) color))
                                                                 cards))
                                                        others-cards)
                                                #'<)))
                            (if (> min n)
                                (push (cons (- min n) my-cards) rule-three-points-and-cards))))))
      (block result
        ;; rule one
        (when winning-cards
          (apply-rule-one)
          (let ((remaining-winning-cards
                  (loop for card in winning-cards
                        unless (find (card-color card) rule-one-donot-throw-colors)
                          collect card)))
            (when remaining-winning-cards
              (return-from result (u:random-elt remaining-winning-cards)))))
        ;; rule two and three
        (apply-rule-two)
        (apply-rule-three)
        (let* ((winning-spades (count +spade+ winning-cards :key #'card-color))
               (all-spades (length my-spades-sorted))
               (max-points 0)
               (strategy nil)
               (card nil))
          (loop for (extra-points . spades-required) in rule-two-points-and-spades
                for points = (+ (min spades-required winning-spades)
                                extra-points)
                do (when (> points max-points)
                     (setf max-points points
                           card (or (find +spade+ winning-cards :key #'card-color)
                                        (u:extrema my-spades-sorted
                                                   #'< :key #'card-number))
                           strategy :rule-2)))
          (loop for (spades-required . cards) in rule-three-points-and-cards
                for points = (min all-spades (+ spades-required winning-spades))
                when (<= spades-required all-spades)
                  do (when (> points max-points)
                       (setf max-points points
                             card (u:random-elt cards)
                             strategy :rule-3)))
          (when card
            (return-from result card)))
        ;; rule 4
        (when winning-cards
          (return-from result (if (find +spade+ winning-cards :key #'card-color :test-not #'=)
                                  (u:random-elt (remove +spade+ winning-cards :key #'card-color))
                                  (u:extrema winning-cards #'< :key #'card-number))))
        ;; rule 5
        (return-from result
          (or (u:extrema (remove-if #'spade-card-p valid-moves) #'< :key #'card-number)
                    (car (last my-spades-sorted))))))))

(defun play-move-v4 (cards hcc hsc turns)
  (if (= (length turns) 4) ;; I am the first player
      (play-opening-move-v4 cards turns)
      (play-move-v2 cards hcc hsc turns)))

(declaim (ftype (function * (simple-array fixnum (4))) simulate-new-game))
(defun simulate-new-game (cards first-playeri)
  (declare (optimize (speed #.*speed*) (debug #.*debug*) (safety #.*safety*)))
  (let ((points (make-array 4 :element-type 'fixnum)))
    (labels ((simulate-round* (turns hcc hsc winner)
               (if (null turns)
                   winner
                   (let ((playeri (car turns)))
                     (multiple-value-bind (card win?) (play-move-v4 cards hcc hsc turns)
                       (setf (nth playeri cards) (remove card (nth playeri cards) :test #'card-=))
                       (multiple-value-bind (hcc hsc) (new-hcc-hsc card hcc hsc)
                         (simulate-round* (rest turns)
                                          hcc hsc
                                          (if win? (car turns) winner)))))))

             (simulate-round (playeri)
               (simulate-round* (loop repeat 4
                                      for i fixnum from playeri
                                      collect (mod i 4))
                                nil nil
                                playeri)))
      (loop repeat 13
            for winner = (simulate-round first-playeri)
            do
               (setf first-playeri winner)
               (incf (aref points winner)))
      points)))
;; deterministic play
(defun remove-nth (n list)
  (cond ((= n 0)
         (cdr list))
        (t (let ((pos (nthcdr (1- n) list)))
             (setf (cdr pos) (cddr pos)))
           list)))

(defun insert-at (item n list)
  (cond ((= n 0) (cons item list))
        (t (let ((pos (nthcdr (1- n) list)))
             (setf (cdr pos) (cons item (cdr pos))))
           list)))

(defun random-combination (cards length)
  (cond ((= length 0) nil)
        ((= length (length cards)) cards)
        ((> length (length cards)) (throw 'backtrack nil))
        (t
         (let ((card (u:random-elt cards)))
           (cons card
                 (random-combination (remove card cards) (1- length)))))))

(defun random-cards0 (cards this-many used-cards)
  "cards is list of set of cards i.e. a set for each player"
  (declare (optimize (debug 3)))
  (cond ((null cards) nil)
        (t
         (let* (;; first choose cards for the set which degree of freedom is least
                (index (loop for card in cards
                             for needed in this-many
                             for has = (length (set-difference card used-cards))
                             for dof = (- has needed)
                             for i from 0
                             with min-index = 0
                             with min = 14
                             do (when (< dof min)
                                  (setf min-index i
                                        min dof))
                             finally (return min-index)))
                (cards1 (random-combination (set-difference (nth index cards) used-cards)
                                            (nth index this-many)))
                (other-cards (random-cards0 (remove-nth index cards)
                                            (remove-nth index this-many)
                                            (concatenate 'list cards1 used-cards))))
           (insert-at cards1 index other-cards)))))

(defun unsure-cards-and-number (dts played-cards-counts)
  (let* ((card-exists (make-array 52 :element-type 'bit))
         (unsure-cards (loop for dt in dts
                             for i from 0
                             collect (loop for card from 0 below 52
                                           when (and (not (= (aref dt card) 1))
                                                     (not (= (aref dt card) 0)))
                                             collect card
                                           do (when (not (= (aref dt card) 0))
                                                (setf (aref card-exists card) 1)))))
         (this-many (loop for dt in dts
                          for played-cards-count in played-cards-counts
                          collect (- 13 played-cards-count (count-if (u:partial #'= 1) dt)))))
    (values unsure-cards this-many)))

(defun map-random-cards (n function cards this-many)
  (assert (every (u:partial #'<= 0) this-many))
  (assert (= (length (remove-duplicates (u:flatten cards))) (reduce #'+ this-many)))
  (let ((count 0))
    (loop repeat n
          do
             (catch 'backtrack
               (funcall function (random-cards0 (copy-seq cards) (copy-seq this-many) nil))
               (incf count)))
    (print (list count n))
    (cond ;; ((and (= count 0) (not (= n 0)))
          ;;  ;; tried but didn't find even a single way
          ;;  ;; proabably this is a bug at some other part of code
          ;;  (print "WARNING: PROBABLE BUG!!"))
          ((< count (* 2/3 n))
           (map-random-cards (- n count) function cards this-many)))))

(defun generate-random-games0 (n function dts played-cards-count)
  (multiple-value-bind (unsure-cards this-many)
      (unsure-cards-and-number dts played-cards-count)
    (print (list unsure-cards this-many))
    (let ((sure-cards (loop for dt in dts
                            collect (loop for card from 0 below 52
                                                when (= (aref dt card) 1)
                                                  collect (card-deindex card)))))
      (map-random-cards n #'(lambda (cards-set)
                              (let* ((prob 1)
                                     (cards (loop for cards1 in cards-set
                                                  for dt in dts
                                                  for cards2 in sure-cards
                                                  do (loop for card in cards1
                                                           do (setf prob (* prob (aref dt card))))
                                                  collect (concatenate 'list (mapcar #'card-deindex cards1) cards2))))
                                (funcall function cards prob)))
                        unsure-cards this-many))))

(defun generate-random-games (n function game)
  (let ((dts (loop for player across (game-players game)
                   collect (player-card-prob player)))
        (played-cards-counts (loop for player across (game-players game)
                                   collect (length (getf (player-data player) :played-cards)))))
    (generate-random-games0 n function dts played-cards-counts)))

(defun simulate-game (cards hands playeri played-cards)
  (let ((points (list 0 0 0 0))
        first-card)
    (labels ((simulate-round* (turns hcc hsc winner)
               (if (null turns)
                   winner
                   (let ((playeri (car turns)))
                     (multiple-value-bind (card win?) (play-move cards hcc hsc turns)
                       (setf (nth playeri cards) (remove card (nth playeri cards) :test #'card-=))
                       (unless first-card (setf first-card card))
                       (multiple-value-bind (hcc hsc) (new-hcc-hsc card hcc hsc)
                         (simulate-round* (rest turns)
                                                 hcc hsc
                                                 (if win? (car turns) winner)))))))

             (simulate-round (playeri played-cards)
               (multiple-value-bind (hcc hsc) (hcc-hsc played-cards)
                 (simulate-round* (loop repeat (- 4 (length played-cards))
                                        for i from playeri
                                        collect (mod i 4))
                                  hcc hsc
                                  playeri))))
      (loop repeat hands
            for played-cards* = played-cards then nil
            for winner = (simulate-round playeri played-cards*)
            do
               (setf first-playeri winner)
               (incf (nth winner points)))
      (list first-card points))))


;;;; Entry Point Functions

;;; Bidding
(defparameter *points* nil)
(defun bid (game)
  (declare (optimize (speed #.*speed*) (debug #.*debug*) (safety #.*safety*)))
  (flet ((average-points (n my-cards myi starting-playeri)
           (declare (type fixnum n myi starting-playeri))
           (d! (setf *points* nil))
           (let ((sum (make-array 4 :element-type 'fixnum))
                 (my-cards (coerce my-cards 'list)))
             (declare (type (vector fixnum 4) sum))
             (loop repeat n
                   for cards = (generate-random-game my-cards myi)
                   for points = (simulate-new-game cards starting-playeri)
                   do  (map-into sum #'+ sum points)
                       (d! (push points *points*)))
             (map '(simple-array single-float 1) (lambda (p)
                                                   (declare (type fixnum p))
                                                   (coerce (/ (* 1.0e0 p) n) 'single-float))
                  sum))))
    (measuring-time (0 1)
      (let* ((first-bidder (loop with players = (game-players game)
                                 with zero = nil
                                 for i from 0 to 4
                                 for bid = (player-bid (svref players (mod i 4)))
                                 when (= bid 0) do (setf zero t)
                                   when (and zero (not (= 0 bid)))
                                     return (mod i 4)))
             (myi (player-id (game-me game)))
             (first-playeri (or first-bidder myi))
             (average-points (average-points 400 (player-cards (game-me game)) myi first-playeri)))
        (declare (type (simple-array single-float 1) average-points))
        (d! (print average-points))
        (let ((fair-bid 99))
          (declare (type fixnum fair-bid))
          (when (= (game-round game) 5)
            (let* ((probable-max-expected-scores
                     (map '(simple-array single-float 1) (lambda (avg score bid)
                                  (declare (type single-float avg score)
                                           (type fixnum bid))
                                  (if (= bid 0) ;; if not bid
                                      (+ avg score 1)
                                      ;; few points extra for safety
                                      (+ bid score (* 0.1 (- bid avg)) 0.2)))
                             average-points
                             (map 'vector #'player-score (game-players game))
                             (map 'vector #'player-bid (game-players game))))
                   (max-expected-from-others (loop for s across probable-max-expected-scores
                                                   for i fixnum from 0
                                                   unless (= i myi)
                                                     maximizing s single-float))
                   (my-expected (+ (aref average-points myi) (player-score (game-me game)))))
              (d! (print probable-max-expected-scores))
              ;; no need to bid more points than highest expected of others to win them
              (when (> my-expected max-expected-from-others)
                (setf fair-bid (max (the fixnum (ceiling (- max-expected-from-others (player-score (game-me game)))))
                                    (the fixnum (floor (- (aref average-points myi) 1e0))))))))

          ;; don't ever bet more than what is expected
          ;; deduct 0.2 points for safety
          (setf fair-bid (min (the fixnum (floor (- (aref average-points myi) 0.2e0)))
                              fair-bid))
          ;; if some one has bid 8 points then you bid 3 points less and try to dhoos him
          (when (find 8 (game-players game) :key #'player-bid)
            (setf fair-bid (- fair-bid 3)))
          (d! (print fair-bid))
          (values (u:clamp fair-bid 1 8)
                  average-points))))))

;;; Inference

(defun infer-from-play (player card played-cards game)
  (declare (optimize (speed #.*speed*) (debug #.*debug*) (safety #.*safety*))
           (type player player)
           (type card card)
           (type (simple-array card 1) played-cards))
  (remove-card-from-game game card)
  (push card (getf (player-data player) :played-cards))
  (when *print-inference*
    (format t "~&~10a played ~a: (~{~a ~})" (player-name player) (encode-card-pretty card)
            (map 'list #'encode-card-pretty played-cards)))
  (when (= (length played-cards) 0)
    (return-from infer-from-play))
  (let* ((first-card-color (card-color (aref played-cards 0)))
         (highest-color-card (highest-card played-cards first-card-color))
         (highest-spade (highest-card played-cards +spade+)))
    (flet ((not-winning-card-implication (highest-card card)
             ;; throwing a not winning card implies that
             ;; the player has no higher cards
             (player-has-not player (range-of-cards (1+ (card-number highest-card))
                                                    +highest-card-number+
                                                    (card-color card))
                             game)
             ;; and also probably it doesn't have any smaller card that it could threw
             (player-has-not player
                             (range-of-cards +lowest-card-number+ (1- (card-number card)) (card-color card))
                             game
                             ;; hyperparameter
                             :probability 0.9)))

      (cond
        ;; if player plays different color card
        ((not (= first-card-color (card-color card)))
         ;; the player has no card of that color
         (player-has-not player (all-cards first-card-color) game)
         ;; and if its not even spade,
         (when (not (= +spade+ (card-color card)))
           ;; then the player has no spade greater than the highest spade played
           (player-has-not player (range-of-cards (if highest-spade (1+ (card-number highest-spade)) +lowest-card-number+)
                                                  +highest-card-number+ +spade+)
                           game)
           ;; also probably didn't have any other smaller card (of that color?), (of any other color?)
           ;; NOTE: any such rule seems to assume too much
           ))

        ;; player play the same color card but not the card that can win
        ((< (card-number card) (card-number highest-color-card))
         ;; then unless spade has been played, he doesn't have higher card
         (if (or (= first-card-color +spade+)
                 (null highest-spade))
             (not-winning-card-implication highest-color-card card))))

      ;; player played spade when it wasn't spade turn
      (when (and (= (card-color card) +spade+)
                 (not (= first-card-color +spade+)))
          ;; and the spade was smaller
          (when (and highest-spade (< (card-number card) (card-number highest-spade)))
            (not-winning-card-implication highest-spade card))))))

(defun infer-from-previous-moves (game)
  (declare (optimize (speed #.*speed*) (debug #.*debug*) (safety #.*safety*)))
  (loop for play across (game-history game)
        do
           (loop for i fixnum from (history-first play)
                 for card across (history-moves play)
                 for player = (svref (game-players game) (mod i 4))
                 for c from 0
                 do
                    (infer-from-play player card
                                     (subseq (history-moves play) 0 c)
                                     game)))
  ;; analyze know portion of this hand
  (loop for card across (game-played-cards game)
        for i fixnum from 0
        with myid = (player-id (game-me game))
        with first-player = (mod (- myid (length (game-played-cards game))) 4)
        for playeri fixnum from first-player
        do
           (infer-from-play (svref (game-players game) (mod playeri 4))
                            card
                            (subseq (game-played-cards game) 0 i)
                            game)))

;;; Play your trick
(declaim (ftype (function nil (simple-array single-float (14))) response-time-table))
(defun response-time-table ()
  (declare (optimize (speed #.*speed*) (debug #.*debug*) (safety #.*safety*)))
  (let ((cumulative-max (make-array 14 :element-type 'single-float)))
    (loop for i fixnum from 0 to 13
        for time fixnum across *time-for-response*
        for count single-float across *time-for-response-count*
        with max single-float = 0.0e0
        do
           (unless (or (= 0 count) (= i 0))
             (setf max (max (/ time count #.(/ internal-time-units-per-second 1000))
                            max)))
           (unless (= i 0)
             (setf (aref cumulative-max (- i 1)) max)))
    (loop for i from 1 to 13 do
      (incf (aref cumulative-max i) (aref cumulative-max (- i 1))))
    cumulative-max))

;; (defun max-iters (tricks-left)
;;   (cond ((<= 11 tricks-left 13) ;;3
;;          8000)
;;         ((<= 8 tricks-left 10)  ;;3
;;          5500)
;;         ((<= 5 tricks-left 7)   ;; 3
;;          4000)
;;         ((<= 1 tricks-left 4)  ;; 4
;;          4000)))

(defun max-iters (tricks-remaining)
  6000)

(defun play-your-trick (game)
  (declare (optimize (speed #.*speed*) (debug #.*debug*) (safety #.*safety*)))
  (let* ((tricks-remaining (length (player-cards (game-me game))))
         (table (response-time-table))
         (expected-time-required (+ (aref table tricks-remaining)
                                    (* *avg-overhead* (1- tricks-remaining))))
         (allowed-time (game-time-remaining game))
         (time-factor* (cond ((= 0 (aref table tricks-remaining)) 1.0e0)
                             (t (1+ (/ (- allowed-time expected-time-required)
                                       (aref table tricks-remaining))))))
         (time-factor (max 0.6 (min time-factor* 1.2)))
         (only-sensible-move (only-sensible-move game)))
    (d!
      (format t "~&expected-time-required: ~d~%" expected-time-required)
      (format t "~&actual-time-left: ~d~%" (game-time-remaining game))
      (format t "~&time-factor: ~,2f~%" time-factor))

    ;; when in hurry play a random valid move
    (when (< (game-time-remaining game)
             (+ (* 1.5 *avg-overhead* tricks-remaining)
                15
                (* 10 (1- tricks-remaining))
                (/ (+ *mcts-overhead* (* *total-time* (/ *total-iterations*) (max-iters tricks-remaining)))
                   #.(/ internal-time-units-per-second 1000))))
      (d! (format t "played in a hurry: ~a remaining" (game-time-remaining game)))
      (let ((card (or (only-sensible-move game)

                      (let ((card nil)
                            (turns (loop repeat (- 4 (length (game-played-cards game)))
                                         for i from (player-id (game-me game))
                                         collect (mod i 4))))
                        (infer-from-previous-moves game)
                        (generate-random-games 1
                                               (lambda (cards prob)
                                                 (declare (ignore prob))
                                                 (multiple-value-bind (hcc hsc) (hcc-hsc (game-played-cards game))
                                                   (print card)
                                                   (setf card (play-move-v4 cards hcc hsc turns))))
                                               game)
                        card)

                      ;; (u:random-elt (valid-moves* (coerce (player-cards (game-me game)) 'list)
                      ;;                             (game-played-cards game)))
                      )))
        (return-from play-your-trick card)))

    (measuring-time (tricks-remaining time-factor)
      ;; then play your hand
      (cond (only-sensible-move
             (d! (print "only one sensible move possible!! "))
             only-sensible-move)
            (t
             ;; analyze previous hands
             (infer-from-previous-moves game)
             (let* ((card (play-your-trick6 game time-factor tricks-remaining)))
               (remove-card-from-game game card)
               (push card (getf (player-data (game-me game)) :played-cards))
               card))))))

(defun play-your-trick6 (game time-factor tricks-remaining)
  (information-state-monte-carlo-tree game 2400 (* time-factor (max-iters tricks-remaining))))


;;;; API & Server
(h:define-easy-handler (hi :uri "/hi") ()
  (setf (h:header-out "Access-Control-Allow-Origin") "*")
  (with-output-to-string (str)
    (yason:encode-alist '(("value" . "hello")) str)))

(h:define-easy-handler (bid-route :uri "/bid")
    ()
  (setf (h:header-out "Access-Control-Allow-Origin") "*")
  (setf (h:header-out "Access-Control-Allow-Methods") "POST,GET,OPTIONS,DELETE,PUT")
  (setf (h:header-out "Access-Control-Allow-Headers") "x-requested-with, Content-Encoding, Content-Type, origin, authorization, accept, client-security-token")
  (when (eql (h:request-method*) :post)
    (let ((game (parse-json (h:raw-post-data :force-text t))))
      (with-output-to-string (str)
        (yason:encode-alist `(,(cons "value" (bid game))) str)))))

(defparameter *previous-game* nil)
(defparameter *expected-remaining-time* nil)

(defun game-continuation-p (game parent-game)
  (declare (optimize (speed #.*speed*) (debug #.*debug*) (safety #.*safety*)))
  (macrolet ((fail ()
               `(progn
                  (return-from game-continuation-p nil)))
             (subset? (subset set)
               (u:once-only (set)
               `(loop for el across ,subset
                      unless (find el ,set :test #'card-=)
                        return nil
                      finally (return t)))))
    (unless (string-equal (player-name (game-me game)) (player-name (game-me parent-game)))
      (fail))
    (if (= (length (game-history game)) 0)
        (fail))
    (loop for h1 across (game-history parent-game)
          for h2 across (game-history game) do
            (unless (history-= h1 h2)
              (fail)))
    (unless (= (length (game-history game)) (1+ (length (game-history parent-game))))
      (fail))
    (loop for c1 across (game-played-cards parent-game)
          for c2 across (history-moves (aref (game-history game) (length (game-history parent-game)))) do
            (unless (card-= c1 c2)
              (fail)))

    (unless (subset? (player-cards (game-me game))
                     (player-cards (game-me parent-game)))
      (fail)))
  t)

(h:define-easy-handler (play :uri "/play")
    ()
  (setf (h:header-out "Access-Control-Allow-Origin") "*")
  (setf (h:header-out "Access-Control-Allow-Methods") "POST,GET,OPTIONS,DELETE,PUT")
  (setf (h:header-out "Access-Control-Allow-Headers") "x-requested-with, Content-Encoding, Content-Type, origin, authorization, accept, client-security-token")
  (when (eql (h:request-method*) :post)
    (let ((sum (reduce #'+ (u:take 10 *overhead-data*)))
          (count (min 10 *overhead-count*))
          (game (parse-json (h:raw-post-data :force-text t))))
      ;; avg overhead
      (if (= 0 count)
          (setf *avg-overhead* 5.0e0)
          (setf *avg-overhead* (max 5.0e0 (coerce (/ sum count) 'single-float))))

      ;; overhead book keeping
      (when (and (not (= (length (player-cards (game-me game))) 13))
                 *expected-remaining-time*
                 *previous-game*
                 (game-continuation-p game *previous-game*))
        (let ((overhead (- *expected-remaining-time* (game-time-remaining game))))
          (d! (push overhead *overhead-data*))
          (incf *overhead-count*)))
      (setf *previous-game* game)

      ;; think and play
      (with-output-to-string (str)
        (let ((card (play-your-trick game)))
          (setf *expected-remaining-time* (- (game-time-remaining game) (/ *time-taken-for-thinking* #.(/ internal-time-units-per-second 1000))))
          (yason:encode-alist `(,(cons "value" (encode-card card))) str))))))



(defvar *server* nil)
(defun start (&optional (port 7000))
  (unless *server*
    (setf *server* (make-instance 'h:easy-acceptor :port port)))
  (format t "Started server on port 7000")
  (h:start *server*))

(defun stop ()
  (when *server*
    (h:stop *server*)
    (setf *server* nil)))

(defun main ()
  (start)
  (loop (sleep 1000)))



;; probability and certainty
