(in-package #:metroid)

;; TODO: Right now doesn't check for other types, no theme, etc.

(defconstant +level-path+ "levels/")
(defparameter *cell-size* 2.0)
(defparameter *wall-size* 3.0)

(defvar *current-level* (make-hash-table :test #'equal))

(defun get-cell (x y)
  (gethash (list x y) *current-level*))

(defun set-cell (x y object)
  (setf (gethash (list x y) *current-level*) object))

(defun save-level (filename)
  (with-open-file (s (concatenate 'string +level-path+ filename) :direction :output :if-exists :supersede :if-does-not-exist :create)
    (maphash (lambda (loc room-type)
               (format s "~S ~S~%" loc room-type))
             *current-level*)))

(defun build-room (pos cell-type)
  (when (eq 'ground cell-type)
    (let ((center-x (* *cell-size* (first pos)))
          (center-y (* *cell-size* (second pos))))
      (make-plane center-x 0.0 center-y
                  *cell-size* *cell-size* 'lightgray)
      (when (not (gethash (list (first pos) (1- (second pos))) *current-level*))
        (make-wall center-x
                   (/ *wall-size* 2.0)
                   (- center-y (/ *cell-size* 2.0))
                   *wall-size* *cell-size* 'east-west 'violet))
      (when (not (gethash (list (first pos) (1+ (second pos))) *current-level*))
        (make-wall center-x
                   (/ *wall-size* 2.0)
                   (+ center-y (/ *cell-size* 2.0))
                   *wall-size* *cell-size* 'east-west 'green))
      (when (not (gethash (list (1+ (first pos)) (second pos)) *current-level*))
        (make-wall (+ center-x (/ *cell-size* 2.0))
                   (/ *wall-size* 2.0)
                   center-y
                   *wall-size* *cell-size* 'north-south 'blue))
      (when (not (gethash (list (1- (first pos)) (second pos)) *current-level*))
        (make-wall (- center-x (/ *cell-size* 2.0))
                   (/ *wall-size* 2.0)
                   center-y
                   *wall-size* *cell-size* 'north-south 'orange)))))

(defun load-level (filename)
  (clrhash *current-level*)
  (with-open-file (s (concatenate 'string +level-path+ filename))
    (loop for loc = (read s nil)
          for room-type = (read s nil)
          while room-type
          do (setf (gethash loc *current-level*) room-type)))
  (maphash #'build-room *current-level*))

