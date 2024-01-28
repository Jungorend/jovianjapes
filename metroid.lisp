;;;; metroid.lisp

(in-package #:metroid)

; (fli:register-module "raylib" :file-name "./lib/raylib.dll" :connection-style :immediate) ; Windows
; (fli:register-module "raylib" :file-name "./lib/libraylib.so.450" :connection-style :immediate) ; Linux

(fli:define-c-typedef bool (:boolean :byte))

(fli:define-c-struct color-struct
  (r :byte) (g :byte) (b :byte) (a :byte))

(fli:define-c-struct vector2
  (x :float) (y :float))

(fli:define-c-struct vector3
  (x :float) (y :float) (z :float))

(fli:define-c-struct vector4
  (x :float) (y :float) (z :float) (w :float))

(fli:define-c-struct camera3D
  (position vector3)
  (target vector3)
  (up vector3)
  (fovy :float)
  (projection :int))

(fli:define-foreign-function (draw-fps "DrawFPS")
    ((pos-x :int)
     (pos-y :int))
  :documentation "Draw current FPS")

(fli:define-foreign-function (update-camera-raylib "UpdateCamera")
    ((camera (:reference-pass (:struct camera3D)))
     (mode :int))
  :documentation "Update camera position for the selected mode")

(fli:define-foreign-function (%begin-mode-3d "BeginMode3D")
    ((camera (:struct camera3D)))
  :documentation "Begin 3D mode with custom camera")

(fli:define-foreign-function (end-mode-3d "EndMode3D")
    ()
  :documentation "Ends 3d mode and returns to default 2d orthographic mode")

(fli:define-foreign-function (draw-grid "DrawGrid")
    ((slices :int)
     (spacing :float))
  :documentation "Draw a grid centered at (0 0 0)")

(fli:define-foreign-function (%get-mouse-delta "GetMouseDelta")
  ()
  :result-type (:struct vector2)
  :result-pointer return-result)

(defun get-mouse-delta ()
  (fli:with-dynamic-foreign-objects ()
    (let ((return-result (fli:allocate-dynamic-foreign-object :type 'vector2)))
      (%get-mouse-delta :return-result return-result)
      (list (fli:foreign-slot-value return-result 'x)
            (fli:foreign-slot-value return-result 'y)))))

(defclass camera-3d ()
  ((position :accessor pos :initform '(10.0 10.0 10.0))
   (target :accessor target :initform '(0.0 0.0 0.0))
   (up :accessor up :initform '(0.0 1.0 0.0))
   (fovy :accessor fovy :initform 45.0)
   (projection :accessor projection :initform 0)))

(defgeneric update-camera (camera mode))
(defmethod update-camera ((camera camera-3d) mode)
  (fli:with-dynamic-foreign-objects ()
    (let ((pos (fli:allocate-dynamic-foreign-object :type 'vector3))
          (target (fli:allocate-dynamic-foreign-object :type 'vector3))
          (up (fli:allocate-dynamic-foreign-object :type 'vector3))
          (cam (fli:allocate-dynamic-foreign-object :type 'camera3D)))
      (setf (fli:foreign-slot-value pos 'x) (nth 0 (pos camera))
            (fli:foreign-slot-value pos 'y) (nth 1 (pos camera))
            (fli:foreign-slot-value pos 'z) (nth 2 (pos camera))
            (fli:foreign-slot-value cam 'position :copy-foreign-object nil) pos
            (fli:foreign-slot-value target 'x) (nth 0 (target camera))
            (fli:foreign-slot-value target 'y) (nth 1 (target camera))
            (fli:foreign-slot-value target 'z) (nth 2 (target camera))
            (fli:foreign-slot-value cam 'target :copy-foreign-object nil) target
            (fli:foreign-slot-value up 'x) (nth 0 (up camera))
            (fli:foreign-slot-value up 'y) (nth 1 (up camera))
            (fli:foreign-slot-value up 'z) (nth 2 (up camera))
            (fli:foreign-slot-value cam 'up :copy-foreign-object nil) up
            (fli:foreign-slot-value cam 'fovy) (fovy camera)
            (fli:foreign-slot-value cam 'projection) (projection camera))
      (update-camera-raylib cam mode))))

(defun begin-mode-3d (camera)
  (fli:with-dynamic-foreign-objects ()
    (let ((pos (fli:allocate-dynamic-foreign-object :type 'vector3))
          (target (fli:allocate-dynamic-foreign-object :type 'vector3))
          (up (fli:allocate-dynamic-foreign-object :type 'vector3))
          (cam (fli:allocate-dynamic-foreign-object :type 'camera3D)))
      (setf (fli:foreign-slot-value pos 'x) (nth 0 (pos camera))
            (fli:foreign-slot-value pos 'y) (nth 1 (pos camera))
            (fli:foreign-slot-value pos 'z) (nth 2 (pos camera))
            (fli:foreign-slot-value cam 'position :copy-foreign-object nil) pos
            (fli:foreign-slot-value target 'x) (nth 0 (target camera))
            (fli:foreign-slot-value target 'y) (nth 1 (target camera))
            (fli:foreign-slot-value target 'z) (nth 2 (target camera))
            (fli:foreign-slot-value cam 'target :copy-foreign-object nil) target
            (fli:foreign-slot-value up 'x) (nth 0 (up camera))
            (fli:foreign-slot-value up 'y) (nth 1 (up camera))
            (fli:foreign-slot-value up 'z) (nth 2 (up camera))
            (fli:foreign-slot-value cam 'up :copy-foreign-object nil) up
            (fli:foreign-slot-value cam 'fovy) (fovy camera)
            (fli:foreign-slot-value cam 'projection) (projection camera))
      (%begin-mode-3d cam))))

(defun build-hash-table (table values)
  (loop for entry in values
        do (setf (gethash (first entry) table) (second entry))))

(defparameter +colors+ (make-hash-table))
(setf (gethash 'lightgray +colors+) '(200 200 200 255)
      (gethash 'gray +colors+) '(130 130 130 255)
      (gethash 'darkgray +colors+) '(80 80 80 255)
      (gethash 'yellow +colors+) '(253 249 0 255)
      (gethash 'gold +colors+) '(255 203 0 255)
      (gethash 'orange +colors+) '(255 161 0 255)
      (gethash 'pink +colors+) '(255 109 194 255)
      (gethash 'red +colors+) '(230 41 55 255)
      (gethash 'maroon +colors+) '(190 33 55 255)
      (gethash 'green +colors+) '(0 228 48 255)
      (gethash 'lime +colors+) '(0 158 47 255)
      (gethash 'darkgreen +colors+) '(0 117 44 255)
      (gethash 'skyblue +colors+) '(102 191 255 255)
      (gethash 'blue +colors+) '(0 121 241 255)
      (gethash 'darkblue +colors+) '(0 82 172 255)
      (gethash 'purple +colors+) '(112 31 126 255)
      (gethash 'violet +colors+) '(135 60 190 255)
      (gethash 'darkpurple +colors+) '(112 31 126 255)
      (gethash 'beige +colors+) '(211 176 131 255)
      (gethash 'brown +colors+) '(127 106 79 255)
      (gethash 'darkbrown +colors+) '(76 63 47 255)
      (gethash 'white +colors+) '(255 255 255 255)
      (gethash 'black +colors+) '(0 0 0 255)
      (gethash 'blank +colors+) '(0 0 0 0)
      (gethash 'magenta +colors+) '(255 0 255 255)
      (gethash 'raywhite +colors+) '(245 245 245 255))

(defparameter +camera-modes+ (make-hash-table))
(build-hash-table +camera-modes+
                  '((camera-custom 0)
                    (camera-free 1)
                    (camera-orbital 2)
                    (camera-first-person 3)
                    (camera-third-person 4)))

(defparameter +input-codes+ (make-hash-table))
(build-hash-table +input-codes+
                  '((key-null 0)
                    (key-apostrophe 39) (key-comma 44) (key-minus 45) (key-period 46) (key-slash 47)
                    (key-zero 48) (key-one 49) (key-two 50) (key-three 51) (key-four 52) (key-five 53) (key-six 54) (key-seven 55) (key-eight 56) (key-nine 57)
                    (key-semicolon 59) (key-equal 61)
                    (key-a 65) (key-b 66) (key-c 67) (key-d 68) (key-e 69) (key-f 70) (key-g 71) (key-h 72) (key-i 73) (key-j 74) (key-k 75) (key-l 76) (key-m 77)
                    (key-n 78) (key-o 79) (key-p 80) (key-q 81) (key-r 82) (key-s 83) (key-t 84) (key-u 85) (key-v 86) (key-w 87) (key-x 88) (key-y 89) (key-z 90)
                    (key-left-bracket 91) (key-backslash 92) (key-right-bracket 93) (key-grave 96) (key-space 32) (key-escape 256) (key-enter 257) (key-tab 258)
                    (key-backspace 259) (key-insert 260) (key-delete 261)
                    (key-right 262) (key-left 263) (key-down 264) (key-up 265) (key-page-up 266) (key-page-down 267) (key-home 268) (key-end 269)
                    (key-caps-lock 280) (key-scroll-lock 281) (key-num-lock 282) (key-print-screen 283) (key-pause 284)
                    (key-f1 290) (key-f2 291) (key-f3 292) (key-f4 293) (key-f5 294) (key-f6 295) (key-f7 296) (key-f8 297) (key-f9 298) (key-f10 299) (key-f11 300) (key-f12 301)
                    (key-left-shift 340) (key-left-control 341) (key-left-alt 342) (key-left-super 343) (key-right-shift 344) (key-right-control 345) (key-right-alt 346) (key-right-super 347) (key-kb-menu 348)
                    (key-kp-0 320) (key-kp-1 321) (key-kp-2 322) (key-kp-3 323) (key-kp-4 324) (key-kp-5 325) (key-kp-6 326) (key-kp-7 327) (key-kp-8 328) (key-kp-9 329)
                    (key-kp-decimal 330) (key-kp-divide 331) (key-kp-multiply 332) (key-kp-subtract 333) (key-kp-add 334) (key-kp-enter 335) (key-kp-equal 336)
                    (key-back 4) (key-menu 5) (key-volume-up 24) (key-volume-down 25)))

(defun key-code (code)
  (gethash code +input-codes+))

(fli:define-foreign-function (init-window "InitWindow")
    ((width :int)
     (height :int)
     (title (:reference-pass :ef-mb-string)))
  :documentation "Initialize Window and OpenGL Context")

(fli:define-foreign-function (set-target-fps "SetTargetFPS")
    ((fps :int))
  :documentation "Set Target FPS (maximum)")

(fli:define-foreign-function (window-should-close "WindowShouldClose")
  nil
  :documentation "Check if application should close (KEY_ESCAPE pressed or windows close icon clicked)"
  :result-type :boolean)

(fli:define-foreign-function (close-window "CloseWindow")
  nil
  :documentation "Close window and unload OpenGL context")

(fli:define-foreign-function (begin-drawing "BeginDrawing")
  nil
  :documentation "Setup canvas (framebuffer) to start drawing")

(fli:define-foreign-function (end-drawing "EndDrawing")
  nil
  :documentation "End canvas drawing and swap buffers (double buffering)")

(fli:define-foreign-function (clear-background "ClearBackground")
    ((color (:struct color-struct)))
  :documentation "Set background color (framebuffer clear color)")

(fli:define-foreign-function (key-pressed? "IsKeyPressed")
    ((key :int))
  :result-type bool
  :documentation "Check if a key has been pressed once")

(fli:define-foreign-function (key-up? "IsKeyUp")
    ((key :int))
  :result-type bool
  :documentation "Check if a key is not being pressed")

(fli:define-foreign-function (key-down? "IsKeyDown")
    ((key :int))
  :result-type bool
  :documentation "Check if a key is being pressed")

(fli:define-foreign-function (draw-text "DrawText")
    ((text (:reference-pass :ef-mb-string))
     (pos-x :int)
     (pos-y :int)
     (font-size :int)
     (color (:struct color-struct)))
  :documentation "Draw text (using default font)"
  :module "raylib")

(fli:define-foreign-function (get-key-pressed "GetKeyPressed")
    ()
  :result-type :int
  :documentation "Get key pressed (keycode), call it multiple times for keys queued, returns 0 when the queue is empty")

(defun set-background-color (color)
  (fli:with-dynamic-foreign-objects ()
    (let ((color-struct (fli:allocate-dynamic-foreign-object :type 'color-struct))
          (color-values (gethash color +colors+)))
      (setf (fli:foreign-slot-value color-struct 'r) (nth 0 color-values)
            (fli:foreign-slot-value color-struct 'g) (nth 1 color-values)
            (fli:foreign-slot-value color-struct 'b) (nth 2 color-values)
            (fli:foreign-slot-value color-struct 'a) (nth 3 color-values))
      (clear-background color-struct))))

(defun set-text (text pos-x pos-y font-size color)
  (fli:with-dynamic-foreign-objects ()
    (let ((color-struct (fli:allocate-dynamic-foreign-object :type 'color-struct))
          (color-values (gethash color +colors+)))
      (setf (fli:foreign-slot-value color-struct 'r) (nth 0 color-values)
            (fli:foreign-slot-value color-struct 'g) (nth 1 color-values)
            (fli:foreign-slot-value color-struct 'b) (nth 2 color-values)
            (fli:foreign-slot-value color-struct 'a) (nth 3 color-values))
      (draw-text text pos-x pos-y font-size color-struct))))

(defparameter *camera* (make-instance 'camera-3d))

(defun gather-input ()
  (cond
    ((key-down? (key-code 'key-left)) (setf (first (target *camera*)) (- (first (target *camera*)) 0.5)))
    ((key-down? (key-code 'key-right)) (setf (first (target *camera*)) (+ (first (target *camera*)) 0.5)))
    ((key-down? (key-code 'key-d)) (setf (nth 2 (target *camera*)) (+ (nth 2 (target *camera*)) 0.5)))
    ((key-down? (key-code 'key-a)) (setf (nth 2 (target *camera*)) (- (nth 2 (target *camera*)) 0.5)))
    ((key-down? (key-code 'key-up)) (setf (second (target *camera*)) (+ (second (target *camera*)) 0.5)))
    ((key-down? (key-code 'key-down)) (setf (second (target *camera*)) (- (second (target *camera*)) 0.5)))))

(defun render-window ()
  (update-camera *camera* (gethash 'camera-custom +camera-modes+))
  (begin-drawing)
  (set-background-color 'darkpurple)
  (begin-mode-3d *camera*)
  (draw-grid 40 1.0)
  (end-mode-3d)
  (set-text (format nil "Position: ~A | Target: ~A" (pos *camera*) (target *camera*)) 400 600 20 'yellow)
  (set-text (format nil "Mouse Delta: ~A" (get-mouse-delta)) 400 700 20 'yellow)
  (draw-fps 10 10)
  (end-drawing))

(defun main ()
  (init-window 1024 768 "Hello Metroid")
  (set-target-fps 60)
  (loop until (window-should-close)
        do (gather-input)
           (render-window)
        finally
           (close-window)))

(main)
