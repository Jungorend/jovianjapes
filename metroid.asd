;;;; metroid.asd

(asdf:defsystem #:metroid
  :description "Messing around with Raylib"
  :author "Jungy"
  :license  "TBD"
  :version "0.0.1"
  :serial t
  :components ((:file "package")
               (:file "raylib")
               (:file "ecs")
               (:file "metroid")))
