;;;; Renderables
;;;;
;;;; This handles all the code on how to render things to the screen.

(in-project :liminality)

(define-component renderable ())

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


(defgeneric render (render-details entity)
  (:documentation "Renders an object to the screen. Should be called within drawing methods"))

(defmethod render ((render-details renderable/plane) entity)
  "Renders a 2d plane"
  (let ((pos (pos entity)))
    (draw-plane (list (x pos)
                      (y pos)
                      (z pos))
                (list (height render-details)
                      (width render-details))
                (color render-details))))

(defmethod render ((render-details renderable/wall) entity)
  "Renders a wall via thin rectangle"
  (let ((pos (pos entity))
        (x-length (if (eq 'north-south (orientation render-details))
                      0.1
                      (width render-details)))
        (y-length (height render-details))
        (z-length (if (eq 'north-south (orientation render-details))
                      (width render-details)
                      0.1)))
    (draw-cube (list (x pos)
                     (y pos)
                     (z pos))
               x-length y-length z-length
               (color render-details))))

(defun render-objects (entity)
  "Generic Render system. Returns the renderable component to render."
  (render (get-entity-in-component 'renderable entity) entity))

(define-system 'render-objects 'renderable)
