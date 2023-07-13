(in-package :cl-callbreak-bot)

(defparameter *determinations-count* 150)

(deftype player-id ()
  `(integer 0 3))

(deftype cardi ()
  `(integer 0 51))

(deftype card-number ()
  `(integer 2 14))

(deftype card-color ()
  `(integer 0 3))

(deftype cards ()
  `(simple-array (integer 0 51)))

(u:eval-always
  (defparameter *debugging* nil)

  (defmacro dassert (&rest args)
    "assertion while debuggging"
    (when *debugging*
      `(assert ,@args))))

(defstruct node
  (card-player 0 :type player-id)
  (next-player 0 :type player-id)
  (card)
  (card-type nil :type symbol)
  (children (make-array 0 :element-type 'node :fill-pointer 0 :adjustable t) :type (vector node))
  (parent nil :type (or null node))

  (reward 0.0e0 :type single-float)
  (sum-reward-squared 0.0e0 :type single-float)
  (visit-count 0 :type u:non-negative-fixnum)
  (availability-count 0 :type u:non-negative-fixnum)

  (turn 0 :type (integer 0 3))
  (current-color 0 :type card-color)
  (current-hcc 0 :type (integer 0 14))
  (current-hsc 0 :type (integer 0 14))
  (current-winner 0 :type player-id)

  (determination-cards-cache #() :type (simple-array t 1)))

(defmethod print-object ((o node) stream)
  (print-unreadable-object (o stream)
    (format stream "~a plays ~a | (~a children) | ~,3f/~,3f = ~,3f " (node-card-player o) (node-card o) (length (node-children o))
            (node-reward o) (node-visit-count o)
            (unless (= 0 (node-visit-count o))
              (/ (node-reward o) (node-visit-count o))))))


(defun make-move (d move playeri)
  "Destructively modify d, to represent having made a `move' by the `playeri'"
  (declare (optimize (speed #.*speed*) (debug #.*debug*) (safety #.*safety*)))
  (let* ((cards (determination-cards d))
         (count (aref cards playeri (card-color move) 0))
         (color (card-color move))
         (card (card-number move)))
    (d! (assert (> count 0)))
    (loop for i from 1 to count
          with found = nil
          when found
            do (setf (aref cards playeri color (- i 1)) (aref cards playeri color i))
          when (= (aref cards playeri color i) card)
            do (setf found t)
          finally (d! (assert found)))
    (decf (aref cards playeri color 0))
    (loop for i fixnum from 0
          for card across (determination-cards-on-table d)
          do
             (when (= card 52)
               (setf (aref (determination-cards-on-table d) i) (card-index move))
               (return))
          finally (d! (error "not place to put card on table")))
    d))

(defparameter *current-iteration* nil)
(defun UCB-tuned (node)
  "UCB-tuned score of a node"
  (declare (optimize (speed #.*speed*) (debug #.*debug*) (safety #.*safety*)))
  (let ((v (+ (max 0.0 (+ (/ (node-sum-reward-squared node) (node-visit-count node))
                          (- (expt (/ (node-reward node) (node-visit-count node)) 2))))
              (sqrt (/ (* 2.0e0 (log (the u:positive-fixnum *current-iteration*)))
                       (node-visit-count node))))))
    (declare (type u:non-negative-single-float v))
    (+ (/ (node-reward node) (node-visit-count node))
       (* (sqrt (* (/ (log (the u:positive-fixnum (node-availability-count node)))
                      (the u:positive-fixnum (node-visit-count node)))
                   (min 0.25e0 v)))))))


(u:defsubst new-wins?-hcc-hsc (color hcc hsc card)
  (declare (optimize (speed #.*speed*) (debug #.*debug*) (safety #.*safety*))
           (type fixnum color hcc hsc))
  (values (or
           ;; no card played till now
           (and (= 0 hcc) (= 0 hsc))
           ;; no spade played, and has higher same color
           (and (= 0 hsc)
                (= (card-color card) color)
                (> (card-number card) hcc))
           ;; higher spader
           (and (= (card-color card) +spade+)
                (if (= color +spade+)
                    (> (card-number card) hcc)
                    (> (card-number card) hsc))))
          (if (or (= hcc 0)
                  (and (= (card-color card) color)
                       (> (card-number card) hcc)))
              (card-number card)
              hcc)
          (if (or (not (= (card-color card) +spade+))
                  (= color +spade+))
              hsc
              (if (> (card-number card) hsc)
                  (card-number card) hsc))))

(defstruct card&type
  (card (make-card) :type card)
  (type nil :type symbol))


(defun sensible-moves (players-after-me-count d played-cards playeri color hcc hsc)
  (declare (optimize (speed #.*speed*) (debug #.*debug*) (safety #.*safety*))
           (type fixnum players-after-me-count playeri color hcc hsc)
           (type determination-cards d)
           (type played-cards-bit-vector played-cards))
  (let ((cards (make-array 13 :initial-element (make-card&type) :element-type 'card&type :adjustable nil :fill-pointer 0))
        (highest-cards (make-array 4 :initial-element nil :element-type '(or null (integer 2 14)))))
    (labels ((add-cards (color)
               (let ((highest (highest-card color)))
                 (loop for i from 1 to (aref d playeri color 0)
                       with next-consecutive = 0
                       for number = (aref d playeri color i)
                       with winning-pushed = nil
                       with base = (- (* color 13) 2) do
                         (if (or (= next-consecutive number)
                                 (and (> number highest) winning-pushed))
                             nil
                             (progn (vector-push (make-card&type :card (make-card* :color color :number number)
                                                                 :type (if (> number highest) (type-winning-of-color color)))
                                                 cards)
                                    (setf winning-pushed t)))
                         (setf next-consecutive (loop for n from (- number 1) downto 2
                                                      when (= (aref played-cards (+ base n)) 0)
                                                        return n)))))

             (add-lowest-card (color type)
               (let ((count (aref d playeri color 0)))
                 (cond ((= count 0)
                        0)
                       (t (vector-push (make-card&type :card (make-card* :color color :number (aref d playeri color count))
                                                       :type type)
                                       cards)
                          1))))

             (type-lowest-of-color (color)
               (ecase color
                 (0 :lowest0)
                 (1 :lowest1)
                 (2 :lowest2)
                 (3 :lowest3)))

             (type-winning-of-color (color)
               (ecase color
                 (0 :winning0)
                 (1 :winning1)
                 (2 :winning2)
                 (3 :winning3)))

             (highest-card (color)
               (the (integer 0 14)
                    (or (aref highest-cards color)
                        (setf (aref highest-cards color)
                              (loop repeat players-after-me-count
                                    for p fixnum from (1+ playeri)
                                    when (> (aref d (mod p 4) color 0) 0)
                                      maximizing (aref d (mod p 4) color 1))))))

             (add-lowest-higher-card (color least-card type)
               (let ((card-number (loop for i from 1 to (aref d playeri color 0)
                                        for card = (aref d playeri color i)
                                        if (< card least-card)
                                          return minima
                                        else
                                          minimizing card into minima
                                        end
                                        finally (return minima))))
                 (vector-push (make-card&type :card (make-card* :color color :number card-number)
                                              :type type)
                              cards)
                 1)))
      (if (= hcc 0)
          ;; I am first player => can play anything
          ;; winning cards
          (loop for color from 0 to 3 do (add-cards color))
          (let* ((spades? (not (= 0 hsc)))
                 (same-color-cards? (not (= 0 (aref d playeri color 0)))))
            (cond
              ;; if you have same color card, you have to play one of the same color cards
              (same-color-cards?
               ;; if spade has been played, you can play the lowest card of same color (because you are not going to win)
               (if (and spades? (not (= color +spade+)))
                   (add-lowest-card color :lowest-sc)
                   ;; if not either play the a winning card from same color cards or any of the same color cards
                   (let ((higher-cards? (> (aref d playeri color 1) hcc)))
                     (if higher-cards?
                         ;; select the lowest card that can win, (this is only suboptimal when your target is not to increase your point
                         ;; but to dhoos others; then you don't play winning card, you let another one win strategically)
                         ;; otherwise play the lowest higher card
                         (let* ((highest-in-remaining-players (highest-card color))
                                (least-winning-card (max highest-in-remaining-players hcc)))
                           (if (> (aref d playeri color 1) least-winning-card)
                               (add-lowest-higher-card color least-winning-card nil)
                               (add-lowest-higher-card color hcc nil))) ;; :lowest-sc
                         ;; or lowest of same color cards because you are not going to win this round
                         (add-lowest-card color :lowest-sc)))))
              ;; if you have spades, then you can play one of the spades
              ((> (aref d playeri +spade+ 0) 0)
               (let* ((winning-spades? (> (aref d playeri +spade+ 1) hsc)))
                 ;; if you have higher spade, play one of them
                 (if winning-spades?
                     (let* ((highest-in-remaining-players (loop repeat players-after-me-count
                                                                for p0 fixnum from (1+ playeri)
                                                                for p = (mod p0 4)
                                                                when (and (= 0 (aref d p color 0))
                                                                          (> (aref d p +spade+ 0) 0))
                                                                  maximizing (aref d p +spade+ 1)))
                            (least-winning-card (max highest-in-remaining-players hsc)))
                       (if (> (aref d playeri +spade+ 1) least-winning-card)
                           (add-lowest-higher-card +spade+ least-winning-card nil)
                           (add-lowest-higher-card +spade+ hsc nil))) ;; lowest3
                     ;; else any lowest card of any other color
                     (when (= 0 (loop for color from 0 to 2
                                      summing (add-lowest-card color (type-lowest-of-color color)) fixnum))
                       (add-lowest-card +spade+ (type-lowest-of-color +spade+))))))
              ;; and if you don't even have spades, you can play lowest card of any other color
              (t (when (= 0 (loop for color from 0 to 2
                                  summing (add-lowest-card color (type-lowest-of-color color)) fixnum))
                   (add-lowest-card +spade+ (type-lowest-of-color +spade+)))))))
      cards)))

(defun move-node-= (move node)
  (or (eql (node-card node) (card&type-card move))
      (and (card&type-type move)
           (eql (node-card-type node) (card&type-type move)))))

(defun categorize-moves (cards nodes)
  (declare (optimize (speed #.*speed*) (debug #.*debug*) (safety #.*safety*))
           (type (array card&type 1) cards)
           (type (array node) nodes))
  (let ((unexplored (make-array (length cards) :initial-element (make-card&type) :fill-pointer 0))
        (explored (make-array (length cards) :initial-element (make-node-and-card) :fill-pointer 0)))
    (loop for ct across cards
          for node = (find ct nodes :test #'move-node-=)
          do (if node
                 (vector-push (make-node-and-card :node node :card (card&type-card ct)) explored)
                 (vector-push ct unexplored)))
    (values explored unexplored)))

(u:defsubst erase-card-type (moves)
  (declare (optimize (speed #.*speed*) (debug #.*debug*) (safety #.*safety*))
           (type (array card&type 1) moves))
  (loop for m across moves do
    (setf (card&type-type m) nil))
  moves)

(defun check-node (node)
  (when (= (node-turn node) 3)
    (assert (= (node-next-player node) (node-current-winner node)))
    (every #'check-node (node-children node))))

(defstruct node-and-card
  (node (make-node) :type node)
  (card (make-card) :type card))

(defun init-node-cache ()
  (make-array *determinations-count* :initial-element nil :element-type '(or null (array node-and-card 1))))

(declaim (ftype (function * (values fixnum &optional)) select-expand-simulate-backpropagate0))
(defun select-expand-simulate-backpropagate0 (node d first-node? score-function)
  (declare (optimize (speed #.*speed*) (debug #.*debug*) (safety #.*safety*))
           (type (function ((simple-array fixnum 1)) (simple-array single-float 1)) score-function))
  ;; (print "SELECTING")
  ;; (print (list node d))
  (with-slots (current-color current-hcc current-hsc (child-card-player next-player)) node
    (labels ((simulate-backpropagate (node d)
               (random-simulate node d)
               (backpropagate node (funcall score-function (determination-points d)) 1.0e0))

             (exit-from-terminal-node ()
               (backpropagate node (funcall score-function (determination-points d)) 1.0e0)
               (return-from select-expand-simulate-backpropagate0 1))

             (select-node (d node card)
               ;; increment visit count
               (incf (node-visit-count node))
               (make-move d card (node-card-player node))
               ;; increment score if round finish
               (when (= (node-turn node) 3)
                 (loop for card across (determination-cards-on-table d)
                       for i from 0 do
                         (d! (when (= 52 card)
                               (error "there should have been 4 cards on the table")))
                         (setf (aref (determination-played-cards d) card) 1)
                         (setf (aref (determination-cards-on-table d) i) 52))
                 (incf (aref (determination-points d) (node-current-winner node)))))

             (visit-one (node-and-cards)
               (declare (type (array t 1) node-and-cards))
               (let* ((max (or (find 0 node-and-cards
                                     :key (lambda (n&c)
                                            (node-visit-count (node-and-card-node n&c))))
                               (u:extremum node-and-cards #'>
                                           :key (lambda (n&m)
                                                  (ucb-tuned (node-and-card-node n&m))))))
                      (child-node (node-and-card-node max))
                      (child-card (node-and-card-card max)))
                 (select-node d child-node child-card)
                 (select-expand-simulate-backpropagate0 child-node d nil score-function)))

             (new-node (parent-node card type)
               (u:let+ ((this-turn (mod (1+ (node-turn parent-node)) 4))
                        (new-round? (= this-turn 0))
                        ((u:&values wins? hcc hsc) (if new-round?
                                                       (values t (card-number card) 0)
                                                       (new-wins?-hcc-hsc current-color current-hcc current-hsc card)))
                        (winner (if wins? child-card-player (node-current-winner parent-node)))
                        (next-player (if (= this-turn 3) winner (mod (1+ child-card-player) 4))))
                 (make-node :card-player (node-next-player parent-node)
                            :next-player next-player
                            :card card
                            :card-type type
                            :children (make-array 0 :element-type 'node :fill-pointer 0 :adjustable t)
                            :parent parent-node
                            :availability-count 0
                            :visit-count 0

                            :turn this-turn
                            :current-color (if new-round? (card-color card) (node-current-color parent-node))
                            :current-winner winner
                            :current-hcc hcc
                            :current-hsc hsc
                            :determination-cards-cache (init-node-cache)))))

      (let* ((cache (node-determination-cards-cache node))
             (d-id (determination-id d))
             (d-cache (aref cache d-id)))
        (declare (type (or null (array node-and-card 1)) d-cache))
        (cond ((and d-cache (= 0 (length d-cache)))
               ;; a already visited terminal node
               (exit-from-terminal-node))
              ((aref cache d-id) ;; node already visited with this determination
               ;; we visit one of the nodes
               (visit-one (aref cache d-id))
               (loop for n&c across (aref cache d-id)
                     do (incf (node-availability-count (node-and-card-node n&c))))
               1)

              (t ;; we are in a new determination for this node
               (let* ((valid-moves* (if (= (node-turn node) 3)
                                        (sensible-moves 3 (determination-cards d) (determination-played-cards d) child-card-player 0 0 0) ;; new round
                                        (sensible-moves (- 3 (mod (1+ (node-turn node)) 4)) (determination-cards d) (determination-played-cards d) child-card-player current-color current-hcc current-hsc)))
                      (valid-moves (if first-node?
                                       (erase-card-type valid-moves*)
                                       valid-moves*)))
                 (multiple-value-bind (explored unexplored)
                     (categorize-moves valid-moves (node-children node))
                   (declare (type (array card&type 1) unexplored)
                            (type (array node-and-card 1) explored))
                   (cond ((and (= 0 (length unexplored))
                               (= 0 (length explored)))
                          ;; this is terminal node
                          (setf (aref cache d-id) #())
                          (exit-from-terminal-node))

                         ((= 0 (length unexplored))
                          (setf (aref cache d-id) explored)
                          (loop for move of-type node-and-card across explored do
                            (incf (node-availability-count (node-and-card-node move))))
                          (visit-one explored))

                         (t ;; if some moves have not been expanded to child node
                          ;; we expand all of them and explore one
                          (setf (aref cache d-id) explored)
                          (loop for move across unexplored
                                with l = (length unexplored)
                                for i fixnum from 1
                                for card = (card&type-card move)
                                for child = (new-node node card (card&type-type move))
                                do
                                   (vector-push-extend child (node-children node) l)
                                   (vector-push-extend (make-node-and-card :node child :card card)
                                                       (aref cache d-id))
                                   (when (= i 1)
                                     (incf (node-availability-count child))
                                     (select-node d child card)
                                     (simulate-backpropagate child d)))
                          1))))))))))

(defun select-expand-simulate-backpropagate (node d score-function)
  (select-expand-simulate-backpropagate0 node d t score-function))

(deftype determination-cards ()
  `(simple-array (integer 0 14) (4 4 14)))

(defstruct determination
  (id 0 :type fixnum)
  (cards (make-array '(4 4 14) :element-type '(integer 0 14)) :type determination-cards)
  (played-cards (make-array 52 :element-type 'bit) :type (simple-bit-vector 52))
  (cards-on-table (make-array 4 :element-type 'fixnum :initial-element 52) :type (simple-array (integer 0 52)))
  (points (make-array 4 :element-type 'fixnum) :type (simple-array fixnum 1))
  (prob 0.0 :type single-float))

(u:defsubst deep-copy (determination)
  (make-determination :cards (u:copy-array (determination-cards determination))
                      :id (determination-id determination)
                      :played-cards (u:copy-array (determination-played-cards determination))
                      :cards-on-table (u:copy-array (determination-cards-on-table determination))
                      :points (copy-seq (determination-points determination))
                      :prob (determination-prob determination)))

(defun add-card (d card playeri)
  (declare (optimize (speed #.*speed*) (debug #.*debug*) (safety #.*safety*)))
  (let* ((cards (determination-cards d))
         (color (card-color card))
         (number (card-number card))
         (count (aref cards playeri color 0))
         (loc 0))
    (loop for i from count downto 1
          do (if (< (aref cards playeri color i) number)
                 (setf (aref cards playeri color (1+ i)) (aref cards playeri color i))
                 (return (setf loc i))))
    (incf (aref cards playeri color 0))
    (setf (aref cards playeri color (+ 1 loc)) (card-number card))))


(u:defsubst remove-random-card (cards playeri color highest-card)
  (declare (optimize (speed #.*speed*) (debug #.*debug*) (safety #.*safety*))
           (type fixnum playeri color highest-card)
           (type determination-cards cards))
  (let ((count (aref cards playeri color 0)))
    (cond ((= count 0)
           nil)
          (t
           (let* ((high-cards-count (- (loop for i from 1 to count
                                             when (< (aref cards playeri color i) highest-card)
                                               return i
                                             finally (return i))
                                       1))
                  (index (if (= high-cards-count 0)
                             (1+ (random count))
                             (1+ (random high-cards-count))))
                  (card (aref cards playeri color index)))
             (loop for i from index to count
                   do (setf (aref cards playeri color i) (aref cards playeri color (+ i 1))))
             (decf (aref cards playeri color 0))
             (the fixnum card))))))

(u:defsubst remove-any-card (cards playeri)
  (declare (optimize (speed #.*speed*) (debug #.*debug*) (safety #.*safety*))
           (type determination-cards cards)
           (type fixnum playeri))
  (let ((total-count (+ (aref cards playeri 0 0)
                        (aref cards playeri 1 0)
                        (aref cards playeri 2 0)
                        (aref cards playeri 3 0))))
    (cond ((= total-count 0)
           nil)
          (t
           (let* ((index (random total-count)))
             (loop for color from 0 to 3
                   for count = (aref cards playeri color 0)
                   with sum fixnum = 0
                   do
                      (cond ((< index (+ sum count))
                             (let* ((index (1+ (- index sum)))
                                    (card (aref cards playeri color index)))
                               (loop for i from index to count
                                     do (setf (aref cards playeri color i) (aref cards playeri color (+ i 1))))
                               (decf (aref cards playeri color 0))
                               (return-from remove-any-card (values card color))))
                            (t (incf sum count)))))))
    nil))

(defun debug-print (d)
  (let ((cards (determination-cards d)))
    (loop for player from 0 to 3 do
      (format t "~&~a has: " player)
      (loop for color from 0 to 3 do
        (loop for i from 1 to (aref cards player color 0)
              do (format t "~d, " (aref cards player color i)))
        (format t "| ")))
    (format t "~%")))

(defun random-simulate (node d)
  (declare (optimize (speed #.*speed*) (debug #.*debug*) (safety #.*safety*)))
  (labels ((next-player (player)
             (declare (type fixnum player))
             (mod (1+ player) 4))

           (simulate0 (player winner color hcc hsc turn)
             (declare (type fixnum player winner color hcc hsc turn))
             ;; (format t "~&~a playing (~a,~a,~a,~a,~a)~%" player winner color hcc hsc turn)
             ;; (debug-print d)
             (cond ((= 4 turn)
                    (incf (aref (determination-points d) winner))
                    ;; (format t "~d:~a " winner (encode-card-pretty (if (= 0 hsc)
                    ;;                                                   (make-card :color color :number hcc)
                    ;;                                                   (make-card :color +spade+ :number hsc))))
                    (simulate0 winner 0 0 0 0 0))
                   ((= 0 turn)
                    ;; any of the cards
                    (multiple-value-bind (card color) (remove-any-card (determination-cards d) player)
                      (when card
                        (simulate0 (next-player player) player color card 0 (1+ turn)))))
                   (t
                    (u:cond-let card
                      ;; player has color card
                      ((remove-random-card (determination-cards d) player color (if (or (= color +spade+)
                                                                                        (= hsc 0))
                                                                                    hcc
                                                                                    0))
                       (simulate0 (next-player player)
                                  (if (and (> (the fixnum card) hcc) (= 0 hsc))
                                      player winner)
                                  color (max hcc card) hsc (1+ turn)))
                      ;; player has a higher spade
                      ((and (not (= color +spade+))
                            (not (= 0 (aref (determination-cards d) player +spade+ 0)))
                            (> (aref (determination-cards d) player +spade+ 1) hsc)
                            (remove-random-card (determination-cards d) player +spade+ hsc))
                       (simulate0 (next-player player)
                                  (if (> (the fixnum card) hsc) player winner)
                                  color hcc (max hsc card) (1+ turn)))
                      ;; player plays any card
                      (t
                       (multiple-value-bind (card color*) (remove-any-card (determination-cards d) player)
                         (declare (ignore color*))
                         (assert card)
                         (simulate0 (next-player player)
                                    winner color hcc hsc (1+ turn)))))))))
    ;; (format t "~& ~a" (determination-points d))
    ;; (print (list (node-next-player node) (node-current-winner node) (node-current-color node)
    ;;              (node-current-hcc node) (node-current-hsc node) (mod (1+ (node-turn node)) 4)))
    (simulate0 (node-next-player node) (node-current-winner node) (node-current-color node)
               (node-current-hcc node) (node-current-hsc node) (mod (1+ (node-turn node)) 4))
    ;; (format t "~a ~%" (determination-points d))
    ))

(defun backpropagate (node points prob)
  (declare (optimize (speed #.*speed*) (debug #.*debug*) (safety #.*safety*))
           (type single-float prob)
           (type (simple-array single-float (4)) points))
  (if (node-parent node)
      (let ((score (* prob (aref points (node-card-player node)))))
        (declare (type single-float score))
        (assert (not (< score 0)))
        (incf (node-reward node) score)
        (incf (node-sum-reward-squared node) (expt score 2))
        (backpropagate (node-parent node) points prob))))


(defun rank (list predicate)
  (let* ((list* (loop for i from 0
                      for item in list
                      collect (cons item i)))
         (sorted (sort list* predicate :key #'car))
         (l (length list))
         (rank (make-array l :element-type 'integer)))
    (loop for (_ . i) in sorted
          for j from 0
          do (setf (aref rank i) j))
    rank))

(deftype played-cards-bit-vector ()
  `(simple-array bit (52)))

(defun all-played-cards-bit-vector (game)
  (declare (optimize (speed #.*speed*) (debug #.*debug*) (safety #.*safety*)))
  (let ((bit-vector (make-array 52 :element-type 'bit)))
    (loop for h across (game-history game)
          do (loop for card across (history-moves h)
                   do (setf (aref bit-vector (card-index card)) 1)))
    bit-vector))

(defun cards-on-table-vector (game)
  (declare (optimize (speed #.*speed*) (debug #.*debug*) (safety #.*safety*)))
  (let ((vec (make-array 4 :element-type '(integer 0 52) :initial-element 52)))
    (loop for i fixnum from 0
          for card across (game-played-cards game)
          do (setf (aref vec i) (card-index card)))
    vec))

(defun information-state-monte-carlo-tree (game n-min n-max &optional (max-time-units most-positive-fixnum))
  (declare (optimize (speed #.*speed*) (debug #.*debug*) (safety #.*safety*))
           (type fixnum n max-time-units)
           (type game game))
  (format t "~&~a to ~a iterations~%" n-min n-max)
  (let* ((determinations nil)
         (start-time (get-internal-real-time))
         (overhead 0)
         (played-cards (game-played-cards game))
         (me (player-id (game-me game)))
         (root (make-node :next-player me
                          :card nil
                          :children (make-array 0 :element-type 'node :fill-pointer 0 :adjustable t)
                          :determination-cards-cache (init-node-cache)
                          ;; player is at (length played-cards) turn in this hand
                          ;; so root node is played at one turn before that
                          :turn (mod (1- (length played-cards)) 4)))
         (all-played-cards (all-played-cards-bit-vector game))
         (cards-on-table (cards-on-table-vector game))
         (tree (trees:make-binary-tree :normal #'< :key #'car))
         (total-prob 0.0e0))
    (declare (type single-float total-prob))
    (unless (= 0 (length played-cards))
      (loop with color = (card-color (elt played-cards 0))
            for player fixnum from (mod (- (player-id (game-me game))
                                           (length played-cards))
                                        4)
            with hcc = 0
            with hsc = 0
            with winner = 0
            for card across played-cards do
              (multiple-value-bind (wins? hcc* hsc*) (new-wins?-hcc-hsc color hcc hsc card)
                (when wins?
                  (setf winner (mod player 4)))
                (setf hcc hcc*
                      hsc hsc*))
            finally (setf (node-current-color root) color
                            (node-current-hcc root) hcc
                            (node-current-hsc root) hsc
                            (node-current-winner root) winner)))
    (let ((count 0))
      (generate-random-games-1 *determinations-count*
                               (lambda (cards-array prob)
                                 (declare (type single-float prob)
                                          (type (simple-array (array card-index) (4)) cards-array))
                                 (incf total-prob prob)
                                 (let ((d (make-determination :prob (coerce prob 'float)
                                                              :id count
                                                              :played-cards all-played-cards
                                                              :cards-on-table cards-on-table)))
                                   (incf count)
                                   (loop for player-cards across cards-array
                                         for playeri fixnum from 0 do
                                           (loop for card across player-cards do
                                             (add-card d (card-deindex% card) playeri)))
                                   (push d determinations)))
                               game))

    ;; create tree
    (loop for d in determinations
          for p = (/ (determination-prob d) total-prob)
          with sum single-float = 0.0e0 do
            (incf sum p)
            (setf (determination-prob d) p)
            (trees:insert (cons sum d) tree))

    ;; select a node
    (labels ((score-function-point-proportional (points)
               (declare (type (simple-array fixnum (4)) points))
               (let* ((sum (reduce #'+ points)))
                 (declare (type fixnum sum))
                 (if (= sum 0)
                     (map '(simple-array single-float 1) (lambda (p)
                                                           (coerce p 'single-float))
                          points)
                     (map '(simple-array single-float 1)
                          (lambda (p) (/ p 1.0e0 sum))
                          points))))

             (score-function ()
               (let ((8bidder (loop for i fixnum from 0
                                    for p across (game-players game)
                                    when (= (player-bid p) 8)
                                      return i)))
                 (cond (8bidder ;; dhoos the player that bid 8
                        (let ((pmax (- 8 (player-won (aref (game-players game) 8bidder)))))
                          #'(lambda (points)
                              (let ((raw-score (score-function-point-proportional points)))
                                ;; 50% score from points
                                ;; other from dhoosing
                                (if (< (aref points 8bidder) pmax)
                                    (loop for i from 0 below 4 do
                                      (when (not (= i 8bidder))
                                        (setf (aref raw-score i) (+ 0.5 (* 0.5 (aref raw-score i))))))
                                    (loop for i from 0 below 4 do
                                      (when (not (= i 8bidder))
                                        (setf (aref raw-score i) (* 0.5 (aref raw-score i))))))
                                raw-score))))
                       (t
                        (let ((targets (make-array 4
                                                   :element-type 'fixnum
                                                   :initial-contents (loop for p across (game-players game)
                                                                           for me? = (player-me? p)
                                                                           for req = (- (player-bid p) (player-won p))
                                                                           collect req))))
                          #'(lambda (points)
                              (let ((base-score (score-function-point-proportional points)))
                                (loop for p across points
                                      for k fixnum from 0
                                      for req across targets
                                      when (< p req)
                                        ;; when someone cannot score, everyone else who score, get a point
                                        do (loop for i from 0 below 4
                                                 for p across points
                                                 for req across targets
                                                 unless (or (= i k)
                                                            (< p req))
                                                   do (incf (aref base-score i))))
                                base-score))))))))
      (setf *current-iteration* 1)
      (setf overhead (- (get-internal-real-time) start-time))
      (loop with score-function = (score-function) do
        (let* ((d0 (u:random-elt determinations))
               (d (deep-copy d0)))
          (incf *current-iteration* (the fixnum (select-expand-simulate-backpropagate root d score-function)))

          ;; once in a while check for STOP criteria
          (when (= (mod *current-iteration* 16) 0)
            (let* ((nodes (u:sort-new (node-children root) #'> :key #'node-visit-count))
                   (v1 (node-visit-count (elt nodes 0)))
                   (v2 (if (> (length nodes) 1)
                           (node-visit-count (elt nodes 1))
                           0)))
              (when (> (- v1 v2) (* 0.8 (- n-max *current-iteration*)))
                ;; when difference to change the most visited nodes is greater than
                ;; the iterations we will be exploring, we can be sure to end early
                (format t "stopping due to STOP ciriteria ~a vs ~a ; ~a exhausted out of ~a"  v1 v2 *current-iteration* n-max)
                (return))))

          ;; Checking for time limit
          (when (and (> (- (get-internal-real-time) start-time)
                        max-time-units)
                     (> *current-iteration* 10))
            (print "stopping due to time limit")
            (return))
          ;; checking for max iteration
          (when (> *current-iteration* n-max)
            (return))))
      (incf *total-iterations* *current-iteration*)
      (incf *total-time* (- (get-internal-real-time) start-time overhead))
      (setf *mcts-overhead* (/ (+ (* *mcts-overhead* *mcts-overhead-runs*) overhead)
                               (incf *mcts-overhead-runs*))))
    ;; (let ((*current-iteration* n))
    ;;   (visualize-tree root :depth 2))
    (let ((card (node-card (u:extrema (node-children root) #'> :key #'node-visit-count))))
      ;; (summarize-tree root #'node-children)
      ;; (map 'nil #'assumed-line-of-play (node-children root))
      ;; (print (encode-card-pretty card))
      (values card
              root))))

(defun visualize-tree0 (node render-function &optional (offset 0) (depth -1))
  (let ((str (funcall render-function node)))
    (format t "~a~a~%" (make-string offset :initial-element #\Space) str)
    (when (not (= 0 depth))
      (loop for child across (node-children node) do
        (visualize-tree0 child render-function (+ offset 5) (1- depth))))))

(defun visualize-tree (node &key (offset 0) (depth -1))
  (visualize-tree0 node (lambda (node)
                          (format nil "~d:~a (~,1f;~a/~a, ~,2f)"
                                  (node-card-player node)
                                  (when (node-card node) (encode-card-pretty (node-card node)))
                                  (node-reward node)
                                  (node-visit-count node) (node-availability-count node)
                                  (unless (= 0 (node-visit-count node)) (ucb-tuned node))))
                   offset depth))

(defun assumed-line-of-play0 (node)
  (format t "~a:~a " (node-card-player node) (encode-card-pretty (node-card node)))
  (let ((card (u:extremum (node-children node) #'> :key #'node-visit-count)))
    (when card
      (assumed-line-of-play0 card))))

(defun assumed-line-of-play (node)
  (format t "~&")
  (assumed-line-of-play0 node)
  (format t "~%"))

(defun summarize-tree (root-node children-key)
  (let ((leaf-nodes 0)
        (non-leaf-nodes 0)
        (max-branching 0)
        (max-height 0))
    (labels ((recurse (node height)
               (let* ((children (funcall children-key node))
                      (l (length children)))
                 (if children
                     (incf non-leaf-nodes)
                     (incf leaf-nodes))
                 (setf max-height (max max-height height))
                 (setf max-branching (max max-branching l))
                 (loop for child in children do
                   (recurse child (+ 1 height))))))
      (recurse root-node 0)
      (format t "~&Branch Factor: ~,2f; Avg. Height: ~,2f; Max. Height: ~a; Max Branch: ~a; ~a Nodes"
              (unless (zerop non-leaf-nodes) (/ (+ non-leaf-nodes leaf-nodes -1) non-leaf-nodes))
              (unless (zerop leaf-nodes) (/ (+ non-leaf-nodes leaf-nodes -1) leaf-nodes))
              max-height
              max-branching
              (+ non-leaf-nodes leaf-nodes -1)))))
