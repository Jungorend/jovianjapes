;;;; liminality.lisp

(in-package #:liminality)

;;;; TODO: Notify when timer action is done? Potentially so actions can then happen
;;;; TODO: Event handler? Still thinking about how this should work. Maybe should be not in ECS.

(defclass input-event ()
  ((owner :accessor owner :initarg :owner)
   (input-event :accessor input-event :initarg :input-event)))

(defparameter *input-queue* (make-array 0 :element-type 'input-event :adjustable t :fill-pointer 0))

(defun check-keyboard-input ()
  "Handles player input by adding keyboard checks to the input queue."
  (when (key-pressed? (gethash 'move-forward *key-config*))
    (vector-push-extend (make-instance 'input-event :owner *player* :input-event 'move-forward) *input-queue*))
  (when (key-pressed? (gethash 'move-backward *key-config*))
    (vector-push-extend (make-instance 'input-event :owner *player* :input-event 'move-backward) *input-queue*))
  (when (key-pressed? (gethash 'rotate-left *key-config*))
    (vector-push-extend (make-instance 'input-event :owner *player* :input-event 'rotate-left) *input-queue*))
  (when (key-pressed? (gethash 'rotate-right *key-config*))
    (vector-push-extend (make-instance 'input-event :owner *player* :input-event 'rotate-right) *input-queue*)))

(defstruct vector3
  (x 0.0 :type float)
  (y 0.0 :type float)
  (z 0.0 :type float))

(define-component pos
    ((x :initform 0.0 :accessor x :initarg :x)
     (y :initform 0.0 :accessor y :initarg :y)
     (z :initform 0.0 :accessor z :initarg :z)))

(defun make-plane (x y z height width color)
  (let ((entity (make-entity)))
    (add-component entity 'pos :x x :y y :z z)
    (update-component 'renderable (id entity) (make-instance 'renderable/plane :height height :width width :color color))))

(defun make-wall (x y z height width orientation color)
  (let ((entity (make-entity)))
    (add-component entity 'pos :x x :y :z z)
    (update-component 'renderable (id entity) (make-instance 'renderable/wall :width width :height height :orientation orientation :color color))))

(define-component viewable
    ((target :accessor target :initform (make-vector3))
     (up :accessor up :initform (make-vector3))
     (fovy :accessor fovy :initform 45.0)
     (projection :accessor projection :initform 0)))

(define-component grid-pos
    ((x :accessor x :initform 0)
     (y :accessor y :initform 0)
     (rotation :accessor rotation :initform 'north)))

(define-component translation
    ((target-x :accessor target-x :initarg :target-x)
     (target-y :accessor target-y :initarg :target-y)
     (target-z :accessor target-z :initarg :target-z)
     (state :accessor state :initform 'idle)
     (spd :accessor spd :initarg :spd)))

(define-component orientation
    ((yaw :accessor yaw :initform (/ pi 2))))

(define-component rotation
    ((state :accessor state :initform 'idle)
     (spd :accessor spd :initarg :spd)
     (target :accessor target :initarg :target)))

(defun make-camera (&key (x 0.0) (y 0.0) (z 0.0))
  (let ((entity (make-entity)))
    (add-component entity pos :x x :y y :z z)
    (add-component entity viewable)))

(defparameter *camera* (make-instance 'camera-3d))
(defparameter *player* (make-entity))
(add-component *player* 'grid-pos)
(add-component *player* 'pos)
(add-component *player* 'translation)
(add-component *player* 'orientation)
(add-component *player* 'rotation)

(defun can-move? (entity)
  (and
   (eq 'idle (state (translation entity)))
   (eq 'idle (state (rotation entity)))))

(defun grid-movement ()
  (loop while (> (length *input-queue*) 0)
        for event = (vector-pop *input-queue*) do
          (when (can-move? (owner event))
            (case (input-event event)
              (move-forward
               (let ((translation (translation (owner event)))
                     (pos (pos (owner event))))
                 (setf (state translation) 'moving
                       (spd translation) 4.0
                       (target-z translation) (z pos))
                 (case (rotation (owner event))
                   (east (setf (target-x translation) (+ (x pos) *cell-size*)
                               (target-y translation) (y pos)))
                   (west (setf (target-x translation) (- (x pos) *cell-size*)
                               (target-y translation) (y pos)))
                   (north (setf (target-x translation) (x pos)
                                (target-y translation) (- (y pos) *cell-size*)))
                   (otherwise (setf (target-x translation) (x pos)
                                    (target-y translation) (+ (y pos) *cell-size*))))))
              (move-backward
               (let ((translation (translation (owner event)))
                     (pos (pos (owner event))))
                 (setf (state translation) 'moving
                       (spd translation) 4.0
                       (target-z translation) (z pos))
                 (case (rotation (owner event))
                   (east (setf (target-x translation) (- (x pos) *cell-size*)
                               (target-y translation) (y pos)))
                   (west (setf (target-x translation) (+ (x pos) *cell-size*)
                               (target-y translation) (y pos)))
                   (north (setf (target-x translation) (x pos)
                                (target-y translation) (+ (y pos) *cell-size*)))
                   (otherwise (setf (target-x translation) (x pos)
                                    (target-y translation) (- (y pos) *cell-size*))))))
              (rotate-left
               nil)
              (rotate-right
               nil)))))

(defun grid-movement ()
  (when (key-pressed? (key-code 'key-a))
    (setf (rotation (grid-pos *player*))
          (case (rotation (grid-pos *player*))
            (north 'east)
            (east 'south)
            (south 'west)
            (otherwise 'north)))
    (make-timer *camera* 'yaw (- (/ pi 2)) :spd 4
                                           :callback (make-instance 'callback :func #'enable-system :args '(grid-movement))))
  (when (key-pressed? (key-code 'key-d))
    (setf (rotation (grid-pos *player*))
          (case (rotation (grid-pos *player*))
            (north 'west)
            (west 'south)
            (south 'east)
            (otherwise 'north)))
    (make-timer *camera* 'yaw (/ pi 2) :spd 4
                                       :callback (make-instance 'callback :func #'enable-system :args '(grid-movement)))))

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

(define-component timer ())

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

(define-system 'update-timers 'timer)
(define-system 'grid-movement 'translation 'rotation 'pos)

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


(define-component game-tick ())
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
