(in-package #:ml/utils)

;;; Moving average buffer
(defstruct (moving-average-buffer
            (:conc-name "MA-")
            (:constructor make-moving-average-buffer%))
  (averager nil :type function)
  (adder nil :type function))

(export 'make-moving-average-buffer)
(defun make-moving-average-buffer (size)
  (let* ((count 0)
         (sum 0)
         (buffer (make-array size))
         (pos 0))
    (flet ((add (x)
             (incf sum x)
             (cond ((= count size)
                    (decf sum (aref buffer pos)))
                   (t
                    (incf count)))
             (setf (aref buffer pos) x)
             (setf pos (mod (1+ pos) size)))
           (average ()
             (/ sum count)))
      (make-moving-average-buffer% :adder #'add
                                   :averager #'average))))

(export 'ma-add)
(defun ma-add (ma x)
  (funcall (ma-adder ma) x))

(export 'ma-average)
(defun ma-average (ma)
  (funcall (ma-averager ma)))

;;
(defstruct (ring-buffer (:constructor make-ring-buffer%))
  contents
  size
  start
  count)

(export 'make-ring-buffer)
(defun make-ring-buffer (size)
  (make-ring-buffer% :size size
                     :contents (make-array size)
                     :start 0
                     :count 0))

(export 'insert-item)
(defun insert-item (item rb)
  (with-slots (contents size start count) rb
    (setf (aref contents start) item)
    (setf count (min (1+ count) size))
    (setf start (mod (1+ start) size))))

(export 'delete-item)
(defun delete-item (rb)
  (with-slots (contents size start count) rb
    (when (= 0 count)
      (error "Ring buffer is empty. Can't pop an item"))
    (prog1 (aref contents (mod (- start count) size))
      (decf count))))

(export 'size)
(defun size (rb)
  (ring-buffer-count rb))
