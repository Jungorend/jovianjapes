(in-package #:metroid)

;;;; This is the Entity-Component-System Framework for the game
;;;;
;;;; (make-entity) when you need to create an entity
;;;; (define-component) to create a new component
;;;; (define-system) to declare a system
;;;; (update-component) to update details for a given component

;; TODO: Right now entities are just a list of 0's
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
(defvar *free-entities* nil)
(defvar *components* (make-array 0 :adjustable t :fill-pointer 0))
(defvar *systems* (make-array 0 :adjustable t :fill-pointer 0))
(defvar *current-systems* nil) ; For the systems applicable to the current scene

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

(defun remove-entity (id)
  "Removes the Entity from the components. Checks to ensure it doesn't duplicate availability."
  (unless (member id *free-entities*)
    (push id *free-entities*))
  (setf (aref *entities* id) nil)
  (loop for component across *components*
        do (setf (aref (entities component) id) nil))
  id)

(defun make-entity ()
  (if *free-entities*
      (progn
        (let ((id (pop *free-entities*)))
          (setf (aref *entities* id) 0)
          id))
      (progn
        (vector-push-extend 0 *entities*)
        (let ((length (length *entities*)))
          (loop for component across *components*
                do (vector-push-extend nil (entities component)))
          (- length 1)))))

(defun get-entity-in-component (component id)
  (aref (entities (get-component component)) id))

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

(defun run-current-systems ()
  (mapc #'apply-system *systems*))
