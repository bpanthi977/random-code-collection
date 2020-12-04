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

(defun valid-passportp (passport)
  (let ((bits (make-array 7 :element-type 'bit))
	i)
    (ppcre:do-register-groups (key) ("(\\w{3}):" passport nil :sharedp t)
      (setf i (field-number key))
      (when i 
	(setf (aref bits i) 1)))
    (= (count 1 bits) 7)))

(defun solve1 (&optional (input *input*)) 
  (let ((passports (ppcre:split "\\n\\n" input :sharedp t)))
    (count-if #'valid-passportp passports)))

    
