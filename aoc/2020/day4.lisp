(defpackage :aoc4
  (:use :cl :aoc))

(in-package :aoc4)

(defparameter *input* (input 4 :string))

(defun field-number (key) 
  (alexandria:switch (key :test #'string=)
    ("byr" 0)
    ("iyr" 1)
    ("eyr" 2)
    ("hgt" 3)
    ("hcl" 4)
    ("ecl" 5)
    ("pid" 6)))  

(defun valid-field (field-number value)
  (case field-number
    ((0 1 2) 
     (and (every #'digit-char-p value)
 	  (case field-number
	    (0 (<= 1920 (parse-integer value) 2002))
	    (1 (<= 2010 (parse-integer value) 2020))
	    (2 (<= 2020 (parse-integer value) 2030)))))
    (3 (multiple-value-bind (n i) (parse-integer value :junk-allowed t)
	 (when n 
	   (let ((unit (subseq value i)))
	     (cond 
	       ((string= unit "cm")
		(<= 150 n 193))
	       ((string= unit "in")
		(<= 59 n 76)))))))
    (4 (and (char= #\# (char value 0))
	    (= (count-if (lambda (c) (digit-char-p c 16)) value)
	       (1- (length value)))))
    (5 (find value '("amb" "blu" "brn" "gry" "grn" "hzl" "oth") :test #'string=))
    (6 (= (count-if #'digit-char-p value) 9))))

(defun valid-passportp (passport)
  (let ((bits (make-array 7 :element-type 'bit))
	i)
    (ppcre:do-register-groups (key value) ("(\\w{3}):(\\S+)" passport nil :sharedp t)
      (setf i (field-number key))
      (format t "~&~d ~a : ~a" i key value)
      (when (and i (valid-field i value))
	(format t " valid")
	(setf (aref bits i) 1)))
    (if (= (count 1 bits) 7)
	(progn (format t "~&Valid Password~%~%")
	       )
	(progn
	  (format t "~& Invalid Password~%~%")
	  nil))))

(defun solve1 (&optional (input *input*)) 
  (let ((passports (ppcre:split "\\n\\n" input :sharedp t)))
    (count-if #'valid-passportp passports)))

    
