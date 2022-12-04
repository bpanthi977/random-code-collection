(in-package :aoc)

(defun score (line)
  (let ((c1 (- (char-code (char line 0)) #.(char-code #\A)))
        (c2 (- (char-code (char line 2)) #.(char-code #\X))))
    (+ (+ c2 1)
       (case (mod (- c2 c1) 3)
         (0 3) ;; same => draw
         (1 6) ;; one step ahead in sequence: rock, paper, scissor => win
         (2 0))))) ;; else => loss

(defun solve1 ()
  (reduce #'+
          (input 02 :lines)
          :key #'score))

(defun score2 (line)
  (let* ((c1 (- (char-code (char line 0)) #.(char-code #\A)))
         (win-loss (- (char-code (char line 2)) #.(char-code #\X)))
         (move-i-play (mod (+ c1 win-loss -1) 3)))
    (+ (* 3 win-loss)
       (1+ move-i-play))))

(defun solve2 ()
  (reduce #'+ (input 02 :lines) :key #'score2))
