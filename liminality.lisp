;;;; liminality.lisp

(in-package #:liminality)

;;;; TODO: Notify when timer action is done? Potentially so actions can then happen
;;;; TODO: Event handler? Still thinking about how this should work. Maybe should be not in ECS.

(defstruct vector3
  (x 0.0 :type float)
  (y 0.0 :type float)
  (z 0.0 :type float))

(define-component pos
    ((x :initform 0.0 :accessor x)
     (y :initform 0.0 :accessor y)
     (z :initform 0.0 :accessor z)))

(define-component movement
    ((initial-position :accessor initial-position)
     (speed :accessor spd)
     (target-position :accessor target-position)
     (state :accessor state)))

(define-component 'renderable ())

(defclass renderable () nil
  (:documentation "A base renderable object"))
(defclass renderable/plane (renderable)
  ((height :accessor height :initarg :height)
   (width :accessor width :initarg :width)
   (color :accessor color :initarg :color))
  (:documentation "A 2d plane along the xz axis"))
(defclass renderable/wall (renderable)
  ((color :accessor color :initarg :color)
   (width :accessor width :initarg :width)
   (height :accessor height :initarg :height)
   (orientation :accessor orientation :initarg :orientation))
  (:documentation "A vertical wall that runs either 'north-south or 'east-west"))

(defgeneric render (render-details id)
  (:documentation "Renders an object to the screen. Should be called within drawing methods"))

(defmethod render ((render-details renderable/plane) id)
  "Renders a 2d plane"
  (let ((pos (get-entity-in-component 'pos id)))
    (draw-plane (list (pos-x pos)
                      (pos-y pos)
                      (pos-z pos))
                (list (height render-details)
                      (width render-details))
                (color render-details))))

(defmethod render ((render-details renderable/wall) id)
  "Renders a wall via thin rectangle"
  (let ((pos (get-entity-in-component 'pos id))
        (x-length (if (eq 'north-south (orientation render-details))
                      0.1
                      (width render-details)))
        (y-length (height render-details))
        (z-length (if (eq 'north-south (orientation render-details))
                      (width render-details)
                      0.1)))
    (draw-cube (list (pos-x pos)
                     (pos-y pos)
                     (pos-z pos))
               x-length y-length z-length
               (color render-details))))

(defun make-plane (x y z height width color)
  (let ((id (make-entity)))
    (update-component 'pos id (make-pos :x x :y y :z z))
    (update-component 'renderable id (make-instance 'renderable/plane :height height :width width :color color))))

(defun make-wall (x y z height width orientation color)
  (let ((id (make-entity)))
    (update-component 'pos id (make-pos :x x :y y :z z))
    (update-component 'renderable id (make-instance 'renderable/wall :width width :height height :orientation orientation :color color))))

(define-component 'viewable
    ((target :accessor target :initform (make-vector3))
     (up :accessor target :initform (make-vector3))
     (fovy :accessor fovy :initform 45.0)
     (projection :accessor projection :initform 0)))

(defclass player ()
  ((x :accessor x :initform 0)
   (y :accessor y :initform 0)
   (state :accessor state :initform 'stationary)
   (rotation :accessor rotation :initform 'north)))

(defun make-camera (&key (x 0.0) (y 0.0) (z 0.0))
  (let ((id (make-entity)))
    (update-component 'pos id (make-pos :x x :y y :z z))
    (update-component 'viewable id (make-viewable))))

(defparameter *sensitivity-y* 0.005)
(defparameter *sensitivity-x* 0.005)
(defparameter *camera* (make-instance 'camera-3d))
(defparameter *player* (make-instance 'player))

(defun grid-movement ()
  (when (or (key-pressed? (key-code 'key-a))
            (key-pressed? (key-code 'key-w))
            (key-pressed? (key-code 'key-s))
            (key-pressed? (key-code 'key-d)))
    (disable-system 'grid-movement))
  (when (key-pressed? (key-code 'key-a))
    (setf (rotation *player*)
          (case (rotation *player*)
            (north 'east)
            (east 'south)
            (south 'west)
            (otherwise 'north)))
    (make-timer *camera* 'yaw (- (/ pi 2)) :spd 4
                                           :callback (make-instance 'callback :func #'enable-system :args '(grid-movement))))
  (when (key-pressed? (key-code 'key-d))
    (setf (rotation *player*)
          (case (rotation *player*)
            (north 'west)
            (west 'south)
            (south 'east)
            (otherwise 'north)))
    (make-timer *camera* 'yaw (/ pi 2) :spd 4
                                       :callback (make-instance 'callback :func #'enable-system :args '(grid-movement))))
  (when (key-pressed? (key-code 'key-w))
    (case (rotation *player*)
      (east (make-timer *camera* 'pos *cell-size*
                        :callback (make-instance 'callback :func #'enable-system :args '(grid-movement))
                        :spd 4.0
                        :target-subposition 2))
      (south (make-timer *camera* 'pos *cell-size*
                         :callback (make-instance 'callback :func #'enable-system :args '(grid-movement))
                         :spd 4.0
                         :target-subposition 0))
      (west (make-timer *camera* 'pos (- *cell-size*)
                        :callback (make-instance 'callback :func #'enable-system :args '(grid-movement))
                        :spd 4.0
                        :target-subposition 2))
      (otherwise (make-timer *camera* 'pos (- *cell-size*)
                             :spd 4.0
                             :callback (make-instance 'callback :func #'enable-system :args '(grid-movement))
                             :target-subposition 0))))
  (when (key-pressed? (key-code 'key-s))
    (case (rotation *player*)
      (east (make-timer *camera* 'pos (- *cell-size*)
                        :callback (make-instance 'callback :func #'enable-system :args '(grid-movement))
                        :spd 4.0
                        :target-subposition 2))
      (south (make-timer *camera* 'pos (- *cell-size*)
                         :callback (make-instance 'callback :func #'enable-system :args '(grid-movement))
                         :spd 4.0
                         :target-subposition 0))
      (west (make-timer *camera* 'pos *cell-size*
                        :callback (make-instance 'callback :func #'enable-system :args '(grid-movement))
                        :spd 4.0
                        :target-subposition 2))
      (otherwise (make-timer *camera* 'pos *cell-size*
                             :spd 4.0
                             :callback (make-instance 'callback :func #'enable-system :args '(grid-movement))
                             :target-subposition 0)))))

(defun render-window ()
  (begin-drawing)
  (set-background-color 'black)
  (begin-mode-3d *camera*)
  (apply-system 'render-objects)
  (end-mode-3d)
  (set-text (format nil "Position: ~A, Direction: ~A" (pos *camera*) (rotation *player*)) 200 600 20 'yellow)
  (set-text (format nil "Active Systems: ~A" *current-systems*) 200 625 20 'yellow)
  (set-text (format nil "Yaw: ~A" (yaw *camera*)) 200 650 20 'yellow)
  (draw-fps 10 10)
  (end-drawing))

(defun main ()
  (init-window 1024 768 "Liminality")
  (set-target-fps 60)
  (loop until (window-should-close)
        do (run-current-systems)
           (render-window)
        finally
           (close-window)))

(defclass callback ()
  ((func :accessor func :initarg :func)
   (args :accessor args :initarg :args :initform nil)))

(defclass timer ()
  ((spd :accessor spd :initarg :spd :initform 1.0)
   (start-time :accessor start-time :initarg :start-time)
   (target :accessor target :initarg :target)
   (target-place :accessor target-place :initarg :target-place)
   (target-subposition :accessor target-subposition :initarg :target-subposition)
   (orig-value :accessor orig-value :initarg :orig-value)
   (callback :accessor callback :initarg :callback)
   (new-value :accessor new-value :initarg :new-value)))

(define-component 'timer)

(defun lerp (timer)
  (let ((time-diff (- (get-time) (start-time timer))))
    (if (> (* (spd timer) time-diff)
           1.0)
        (new-value timer)
        (+ (orig-value timer)
            (* (* time-diff (spd timer))
               (- (new-value timer) (orig-value timer)))))))

                                        ; TODO: callback should be created by make-timer to simplify creation
(defun make-timer (target target-place diff &key (spd 1.0) callback (target-subposition nil))
  (let* ((orig-value (if target-subposition
                         (nth target-subposition (slot-value target target-place))
                         (slot-value target target-place)))
         (entity-id (make-entity))
         (timer (make-instance 'timer :spd spd :target target :target-place target-place
                                      :orig-value orig-value
                                      :start-time (get-time)
                                      :target-subposition target-subposition
                                      :callback callback
                                      :new-value (+ diff orig-value))))
    (update-component 'timer entity-id timer)))


(defun make-event (description callback)
  (let ((event (make-entity)))
    (update-component 'event event
                      (make-instance 'event :description description
                                            :callback callback))))

                                        ; TODO: Timers need to be able to also accept non-slot-values
(defun update-timers (timer-id)
  (let* ((timer (get-entity-in-component 'timer timer-id))
         (adjusted-value (lerp timer)))
    (if (target-subposition timer)
        (setf (nth (target-subposition timer)
                   (slot-value (target timer) (target-place timer)))
              adjusted-value)
        (setf (slot-value (target timer) (target-place timer))
              adjusted-value))
    (when (= adjusted-value (new-value timer))
      (when (callback timer)
        (make-event "Timer Completed" (callback timer)))
      (remove-entity timer-id))))

(defun render-objects (id)
  "Generic Render system. Returns the renderable component to render."
  (render (get-entity-in-component 'renderable id) id))

(define-system 'update-timers 'timer)
(define-system 'render-objects 'renderable)
(define-system 'grid-movement)

(enable-system 'update-timers)
(enable-system 'grid-movement)

(define-component event
    ((description :accessor description :initarg :description)
     (callback :accessor callback :initarg :callback)))

(defun process-events (id)
  (let* ((event (get-entity-in-component 'event id))
         (callback (callback event)))
    (if (null (args callback))
        (funcall (func callback))
        (apply (func callback) (args callback)))
    (remove-entity id)))

(define-system 'process-events 'event)
(enable-system 'process-events)


(define-component 'game-tick ())
                                        ; TODO: Make system that events can push to call each game-tick

(defgeneric game-tick (obj) (:documentation "These are the calls that only update every time the player performs an action."))
(defmethod game-tick ((encounter-timer encounter-timer))
  (let ((stage (nth (ticker encounter-timer) (thresholds encounter-timer))))
    (cond ((<= (random 100) (first stage))
           (incf (ticker encounter-timer)))
          ((<= (random 100) (rest stage))
           (progn
             (make-event "Encounter occurred"
                         (make-instance 'callback :func (lambda () '()))) ; TODO: Write actual function
             (setf (ticker encounter-timer) 0)))
          ('otherwise nil))))
