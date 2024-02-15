(in-package #:metroid)
;;; TODO: For now just storing as a list of lists. Probably should be a map.

(defvar *current-level* nil)

(defclass cell ()
  ((cell-type :accessor cell-type :initarg :cell-type)))

(defun make-grid-cell (&optional (type 'ground))
  (make-instance 'cell :cell-type type))

(defun get-cell (x y)
  (second
   (assoc (list x y) *current-level* :test #'equal)))

(defun set-cell (x y place)
  (push `((,x ,y) ,place) *current-level*))
