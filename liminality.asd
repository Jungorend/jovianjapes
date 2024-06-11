;;;; metroid.asd

(asdf:defsystem #:liminality
    :description "Dungeon Crawler with Raylib"
    :author "Jungy"
    :license  "TBD"
    :version "0.0.1"
    :serial t
    :components ((:file "package")
                 (:file "util")
                 (:file "raylib")
                 (:file "ecs")
                 (:file "config")
                 (:file "debug-ui")
                 (:file "leveleditor")
                 (:file "render")
                 (:file "liminality")))
