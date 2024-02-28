(in-package #:metroid)

;; TODO: Right now doesn't check for other types, no theme, etc.

(defconstant +level-path+ "levels/")

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
    (let ((center-x (* 2.0 (first pos)))
          (center-y (* 2.0 (second pos))))
      (make-plane center-x 0.0 center-y
                  2.0 2.0 'lightgray))))

(defun load-level (filename)
  (clrhash *current-level*)
  (with-open-file (s (concatenate 'string +level-path+ filename))
    (loop for loc = (read s nil)
          for room-type = (read s nil)
          while room-type
          do (setf (gethash loc *current-level*) room-type)))
  (maphash #'build-room *current-level*))

