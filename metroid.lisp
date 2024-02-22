;;;; metroid.lisp

(in-package #:metroid)

(defstruct vector3
  (x 0.0 :type float)
  (y 0.0 :type float)
  (z 0.0 :type float))

(define-component 'position)
(defstruct position
  (x 0.0 :type float)
  (y 0.0 :type float)
  (z 0.0 :type float))

(define-component 'renderable)

(defclass renderable () nil
  (:documentation "A base renderable object"))
(defclass renderable/plane (renderable)
  ((height :accessor height :initarg :height)
   (width :accessor width :initarg :width)
   (color :accessor color :initarg :color))
  (:documentation "A 2d plane along the xz axis"))
(defclass renderable/wall (renderable)
  ((height :accessor height :initarg :height)
   (width :accessor width :initarg :width)
   (color :accessor color :initarg :color)
   (orientation :accessor orientation :initarg :orientation))
  (:documentation "A vertical wall that runs either 'north-south or 'east-west"))

(defgeneric render (render-details id)
  (:documentation "Renders an object to the screen. Should be called within drawing methods"))

(defmethod render ((render-details renderable/plane) id)
  "Renders a 2d plane"
  (let ((position (get-entity-in-component 'position id)))
    (draw-plane (list (position-x position)
                      (position-y position)
                      (position-z position))
                (list (height render-details)
                      (width render-details))
                (color render-details))))

(define-system 'render-objects
  (lambda (id) (render (get-entity-in-component 'renderable id) id))
  'renderable)

(defun make-plane (x y z height width color)
  (let ((id (make-entity)))
    (update-component 'position id (make-position :x x :y y :z z))
    (update-component 'renderable id (make-instance 'renderable/plane :height height :width width :color color))))

(define-component 'viewable)
(defstruct viewable
  (target (make-vector3) :type vector3)
  (up (make-vector3) :type vector3)
  (fovy 45.0 :type float)
  (projection 0 :type fixnum))

(defclass player ()
  ((x :accessor x :initform 0)
   (y :accessor y :initform 0)
   (rotation :accessor rotation :initform 'north)))

(defun make-camera (&key (x 0.0) (y 0.0) (z 0.0))
  (let ((id (make-entity)))
    (update-component 'position id (make-coords :x x :y y :z z))
    (update-component 'viewable id (make-viewable))))

(defparameter *sensitivity-y* 0.005)
(defparameter *sensitivity-x* 0.005)
(defparameter *camera* (make-instance 'camera-3d))
(defparameter *player* (make-instance 'player))

(defun grid-movement ()
  (when (key-pressed? (key-code 'key-a))
    (setf (rotation *player*)
          (case (rotation *player*)
            ('north 'east)
            ('east 'south)
            ('south 'west)
            (otherwise 'north))))
  (when (key-pressed? (key-code 'key-d))
    (setf (rotation *player*)
          (case (rotation *player*)
            ('north 'west)
            ('west 'south)
            ('south 'east)
            (otherwise 'north))))
  (when (key-pressed? (key-code 'key-w))
    (setf (pos *camera*) (case (rotation *player*)
                           ('north (list
                                    (first (pos *camera*))
                                    (second (pos *camera*))
                                    (+ (nth 2 (pos *camera*)) 2.0)))
                           ('east (list
                                   (+ (first (pos *camera*)) 2.0)
                                   (second (pos *camera*))
                                   (nth 2 (pos *camera*))))
                           ('south (list
                                    (first (pos *camera*))
                                    (second (pos *camera*))
                                    (- (nth 2 (pos *camera*)) 2.0)))
                           (otherwise (list
                                       (- (first (pos *camera*)) 2.0)
                                       (second (pos *camera*))
                                       (nth 2 (pos *camera*)))))))
  (when (key-pressed? (key-code 'key-s))
    (setf (pos *camera*) (case (rotation *player*)
                           ('north (list
                                    (first (pos *camera*))
                                    (second (pos *camera*))
                                    (- (nth 2 (pos *camera*)) 2.0)))
                           ('east (list
                                   (- (first (pos *camera*)) 2.0)
                                   (second (pos *camera*))
                                   (nth 2 (pos *camera*))))
                           ('south (list
                                    (first (pos *camera*))
                                    (second (pos *camera*))
                                    (+ (nth 2 (pos *camera*)) 2.0)))
                           (otherwise (list
                                       (+ (first (pos *camera*)) 2.0)
                                       (second (pos *camera*))
                                       (nth 2 (pos *camera*)))))))
  (setf (yaw *camera*) (case (rotation *player*)
                         ('west pi)
                         ('north (/ pi 2))
                         ('east 0.0)
                         (otherwise (* (/ 3 2) pi)))))

(defun gather-input ()
  (when (key-pressed? (key-code 'key-z))
    (disable-cursor))
  (when (key-pressed? (key-code 'key-x))
    (enable-cursor))
  (grid-movement))

(defun render-window ()
  (begin-drawing)
  (set-background-color 'darkpurple)
  (begin-mode-3d *camera*)
  (apply-system 'render-objects)
  (draw-grid 40 1.0)
  (draw-cube '(-16.0 2.5 0.0) 1.0 5.0 32.0 'blue)
  (draw-cube '(0.0 2.5 -2.0) 10.0 5.0 0.1 'pink)
  (draw-cube '(0.0 2.5 2.0) 10.0 5.0 0.1 'pink)
  (draw-cube '(16.0 2.5 0.0) 1.0 5.0 32.0 'lime)
  (draw-cube '(0.0 2.5 16.0) 32.0 5.0 1.0 'gold)
  (draw-plane '(0.0 0.0 0.0) '(32.0 32.0) 'lightgray)
  (end-mode-3d)
  (set-text (format nil "Position: ~A" (pos *camera*)) 400 600 20 'yellow)
  (set-text (format nil "Rotation: ~A" (rotation *player*)) 400 650 20 'blue)
  (set-text (format nil "Pitch: ~A | Yaw: ~A" (pitch *camera*) (yaw *camera*)) 400 700 20 'yellow)
  (draw-fps 10 10)
  (end-drawing))

;; TODO: Need to clear existing scene data
(defun load-test-scene ()
  (make-plane 0.0 0.0 0.0 32.0 32.0 'lightgray))

(defun main ()
  (init-window 1024 768 "Hello Metroid")
  (set-target-fps 60)
  (load-test-scene)
  (loop until (window-should-close)
        do (gather-input)
           (render-window)
        finally
           (close-window)))
