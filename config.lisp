(in-package :liminality)

(defparameter *key-config* (make-hash-table :test 'eq))
(setf (gethash 'move-forward *key-config*) (key-code 'key-w)
      (gethash 'move-backward *key-config*) (key-code 'key-s)
      (gethash 'rotate-left *key-config*) (key-code 'key-a)
      (gethash 'rotate-right *key-config*) (key-code 'key-d))
