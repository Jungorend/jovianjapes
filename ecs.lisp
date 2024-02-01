(in-package #:metroid)

;;;; This is the Entity-Component-System Framework for the game
;;;;
;;;; (make-entity) when you need to create an entity
;;;; (define-component) to create a new component
;;;; (define-system) to declare a system
;;;; (update-component) to update details for a given component

;; TODO: make it so that it checks a *free-entities* first
;; TODO: functions to remove entities, components, and systems
;; TODO: make decision whether unused systems should be removed from *systems* and therefore apply systems
;; just iterates through them all
;; This way we can recycle addresses when they are freed rather than exclusively append
;; NOTE: Each System takes exactly 1 argument, which is the id of the entity it's starting with

(defclass component ()
  ((name :accessor name :initarg :name :type 'symbol)
   (entities :accessor entities :initform (make-array 0 :adjustable t :fill-pointer 0))))

(defclass system ()
  ((name :accessor name :initarg :name :type 'symbol)
   (f :accessor f :initarg :f :type 'function)
   (components :accessor components :initarg :components)))

(defvar *entities* (make-array 0 :adjustable t :fill-pointer 0))
(defvar *components* (make-array 0 :adjustable t :fill-pointer 0))
(defvar *systems* (make-array 0 :adjustable t :fill-pointer 0))

(defun define-component (name)
  (let ((component (make-instance 'component :name name)))
    (loop for i upto (- (length *entities*) 1)
          do (vector-push-extend nil (entities component)))
    (vector-push-extend component *components*)))

(defun define-system (name function &rest required-components)
  (let ((sys (make-instance 'system
                         :name name
                         :f function
                         :components required-components)))
    (vector-push-extend sys *systems*)))

(defun get-component (name)
  (find-if (lambda (component)
             (eq name (name component)))
           *components*))

(defun update-component (component id data)
  (let ((component (get-component component)))
    (setf (aref (entities component) id) data)))

(defun make-entity ()
  (vector-push-extend 0 *entities*)
  (let ((length (length *entities*)))
    (loop for component across *components*
          do (vector-push-extend nil (entities component)))
    (- length 1)))

(defun get-entities-in-system (&rest required-components)
  (let ((components (reduce (lambda (results component)
                              (let ((c (get-component component)))
                                (if c
                                    (cons c results)
                                    results)))
                            required-components :initial-value '())))
    (loop for i upto (- (length *entities*) 1)
          when (every (lambda (component)
                        (aref (entities component) i))
                      components)
            collect i)))

(defun apply-system (name)
  (let* ((sys (find-if (lambda (system)
                     (eq name (name system)))
                       *systems*))
         (entities (apply #'get-entities-in-system (components sys))))
    (loop for entity in entities
          do (funcall (f sys) entity))))

#|
;; Testing calls
(define-component 'position)
(define-component 'viewable)
(define-component 'obstacles)
(add-entity)
(add-entity)
(add-entity)
(add-entity)
(update-component 'viewable 2 t)
(update-component 'viewavle 4 t)
(define-system 'display-viewable (lambda (x) (format t "~A~%" x)) 'viewable)
(apply-system 'display-viewable)
#|
