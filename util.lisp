;;;; General utility tools to make other things easier

(in-package :liminality)

(defmacro build-hash-table (table (&key (test 'eql)) &rest values)
  "Simpler hash table declaration. Check examples in raylib.lisp to see it used."
  (let ((entry (gensym)))
    `(progn
       (setf ,table (make-hash-table :test ',test))
       (dolist (,entry ',values)
         (setf (gethash (first ,entry) ,table)
               (second ,entry))))))
