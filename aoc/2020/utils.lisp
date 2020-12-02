(ql:quickload :drakma)

(defpackage :aoc 
  (:use :cl)
  (:export :input))

(in-package :aoc)

(defun url (day) 
  (check-type day (integer 1 *))
  (format nil "https://adventofcode.com/2020/day/~d/input" day))

(defun file (day)
  (check-type day (integer 1 *))
  (pathname (format nil "/home/bpanthi/lisp/rcc/aoc/2020/inputs/~d" day)))


(defparameter +cookie-jar+ 
  (make-instance 
   'drakma:cookie-jar 
   :cookies 
   (list (make-instance
	  'drakma:cookie 
	  :http-only-p t 
	  :securep t 
	  :path "/"
	  :domain ".adventofcode.com"
	  :name "session"
	  :value "youcookievalue"
	  ))))
					
(defun input (day &optional type)
  (let ((file (file day)))
    (if (probe-file file)
	(case type 
	  (:string (uiop:read-file-string file))
	  (:lines (uiop:read-file-lines file))
	  (t (uiop:read-file-form file)))
	(let ((data (drakma:http-request (url day)
					 :cookie-jar +cookie-jar+))
	      (file (file day)))
	  (ensure-directories-exist file)
	  (with-open-file (stream (file day) 
				  :direction :output)
	    (princ data stream))
	  (input day type)))))

