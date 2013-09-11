(ql:quickload :alexandria)
(ql:quickload :cl-fad)
(ql:quickload :lispbuilder-sdl)
(ql:quickload :lispbuilder-sdl-ttf)

(defpackage :dog
  (:use :cl :alexandria)
  (:import-from :cl-fad :list-directory))

(in-package :dog)

(load "util.lisp")
(load "items.lisp")
(load "monsters.lisp")

(defparameter *width* 30
  "Number of horizontal tiles in the world map.")

(defparameter *height* 30
  "Number of vertical tiles in the world map.")

(defparameter *tile-size* 40
  "Pixel size of a tile.")

(defparameter *win-w* 1024
  "Default window width in pixels.")

(defparameter *win-h* 768
  "Default window height in pixels.")

(defparameter *panel-w* 352
  "Width of the panel in pixels.")

(defparameter *bag-size* 32
  "Maximum number of items player can carry.")

(defparameter *fov* nil
  "List of tiles within the field of view.")

(defparameter *p1* nil
  "Player struct.")

(defparameter *vp-x* (truncate (- *win-w* *panel-w*) (* 2 *tile-size*))
  "Horizontal tile offset for player.")

(defparameter *vp-y* (truncate *win-h* (* 2 *tile-size*))
  "Vertical tile offset for player.")

(defparameter *grab-next-key* nil
  "Set to T when the next keypress should be sent as an event to
  *NEXT-ITEM-FUNCTION*.")

(defparameter *next-item-function* nil
  "Function to handle the next keypress event when *GRAB-NEXT-KEY* is T.")

;; ------------------------------------------------------ [ ANIMATION DECLS ]
(defstruct anim
  x y (length 10) (alpha 255) sprite update (x-off 0) (y-off 0))
(defparameter *anims* nil)

;; ---------------------------------------------------------- [ LEVEL DECLS ]
(defstruct level
  (name 'none) (map nil) (discovery nil) (items nil) (monsters nil)
  (stairs nil))
(defparameter *levels* nil)
(defparameter *lev* nil) ; Current level

;; ---------------------------------------------------------- [ PLAYER DECLS]
(defstruct player
  x y hp max-hp lvl xp luck bag hunger weapon armor boots helmet)

(defmethod player-pos (p)
  (cons (player-x p) (player-y p)))

;; ------------------------------------------------------------ [ ANIMATION ]
(defun update-anims ()
  (dolist (a *anims*) (funcall (anim-update a) a))
  (setf *anims* (remove-if (lambda (a) (zerop (anim-length a))) *anims*)))

(defun clear-anims ()
  (setf *anims* nil))

(defun make-damage-anim (x y text)
  (push
   (make-anim
    :x x
    :y y
    :length 15
    :sprite (let ((splat (sdl:copy-surface
                          :surface (sprite 'tile-blood-splat))))
              (sdl:draw-string-blended
               text
               (sdl:point :x (- 19 (* 5 (length text))) :y 5)
               :surface splat
               :color sdl:*white*
               :font *font-large*)
              (setf (sdl:alpha-enabled-p splat) t)
              (sdl:blit-surface splat))
    :update (lambda (a) (decf (anim-length a))))
   *anims*))

(defun make-lvlup-anim (x y)
  (push
   (make-anim
    :x x
    :y y
    :y-off -10
    :x-off -30
    :length 100
    :sprite (let ((tmps (sdl:render-string-solid
                         "Level gain!"
                         :font *font-large*
                         :color (sdl:color :r 255 :g 255 :b 0))))
              (setf (sdl:alpha-enabled-p tmps) t)
              (setf (sdl:color-key-enabled-p tmps) t)
              (sdl:blit-surface tmps))
    :update (lambda (a)
              (decf (anim-length a))
              (decf (anim-y-off a))
              (decf (anim-alpha a) 2)))
   *anims*))

;; --------------------------------------------------------------- [ PLAYER ]
(defun player-grab ()
  "Grab an item."
  (let ((item (object-at 'item (player-x *p1*) (player-y *p1*))))
    (if item
        (grab-item item)
        (add-message "Nothing to grab."))))

(defun player-drop ()
  "Drop an item."
  (if (null (player-bag *p1*))
      (add-message "Nothing to drop.")
      (progn
        (add-message "Drop what?")
        (setf *next-item-function* #'drop-item)
        (setf *grab-next-key* t))))

(defun player-apply ()
  "Apply an item."
  (if (null (player-bag *p1*))
      (add-message "Nothing to apply.")
      (progn
        (add-message "Apply what?")
        (setf *next-item-function* #'apply-item)
        (setf *grab-next-key* t))))

(defun player-inspect ()
  "Inspect an item."
  (if (null (player-bag *p1*))
      (add-message "Nothing to inspect.")
      (progn
        (add-message "Inspect what?")
        (setf *next-item-function* #'inspect-item)
        (setf *grab-next-key* t))))

(defun player-move (dx dy)
  (let ((x* (+ (player-x *p1*) dx))
        (y* (+ (player-y *p1*) dy)))
    (cond ((object-at 'monster x* y*)
           (player-attack (object-at 'monster x* y*)
                          (+ (random-range (player-lvl *p1*)
                                           (+ 5 (player-lvl *p1*)))
                             (if (null (player-weapon *p1*))
                                 0
                                 (damage (player-weapon *p1*))))))
          ((walkable-p (map-ref x* y*))
           (setf (player-x *p1*) x*) (setf (player-y *p1*) y*))))
  (recompute-fov)
  (end-turn))

(defun player-climb ()
  "Move up or down stairs."
  (let ((stairs (cdr (assoc (cons (player-x *p1*) (player-y *p1*))
                            (level-stairs *lev*) :test #'equal))))
    (if stairs
        (destructuring-bind (level direction) stairs
          (progn
            (setf *lev* (lev level))
            (clear-anims)
            (recompute-fov)
            (add-message
             (format nil "Climbed ~:[down~;up~] to ~a."
                     (eq direction 'up) level))))
        (add-message "Can't climb here!"))))

(defun xp-gain (amount)
  (let ((old-lvl (player-lvl *p1*)))
    (incf (player-xp *p1*) amount)
    (setf (player-lvl *p1*) (1+ (truncate (log (player-xp *p1*)))))
    (unless (eq old-lvl (player-lvl *p1*))
      (make-lvlup-anim (player-x *p1*) (player-y *p1*))
      (incf (player-max-hp *p1*) 50)
      (incf (player-hp *p1*) 50)
      (add-message (format nil "You grow to level ~a!" (player-lvl *p1*))))))

(defun player-rest ()
  "Rest for one turn."
  (setf (player-hp *p1*) (min (player-max-hp *p1*) (1+ (player-hp *p1*))))
  (end-turn))

(defun player-attack (m atck)
  (let* ((crit (< (random 100) (player-luck *p1*)))
         (atck (if crit (* atck 3) atck)))
    (add-message (format nil "You hit ~a ~afor ~a damage!"
                         (monster-print-name m)
                         (if crit "critically " "") atck))
    (decf (monster-hp m) atck)
    (make-damage-anim (monster-x m) (monster-y m) (write-to-string atck))))

(defun player-update ()
  (when (zerop (player-hp *p1*))
    (add-message "You die. Game over!"))
  (decf (player-hunger *p1*)))

(defun player-move-l ()
  "Move left."
  (player-move -1 0))

(defun player-move-r ()
  "Move right."
  (player-move 1 0))

(defun player-move-u ()
  "Move up."
  (player-move 0 -1))

(defun player-move-d ()
  "Move down."
  (player-move 0 1))

(defun player-move-ul ()
  "Move up-left."
  (player-move -1 -1))

(defun player-move-ur ()
  "Move up-right."
  (player-move 1 -1))

(defun player-move-dl ()
  "Move down-left."
  (player-move -1 1))

(defun player-move-dr ()
  "Move down-right."
  (player-move 1 1))

;; --------------------------------------------------------- [ GAME CONTROL ]
(defun end-turn ()
  (update-monsters *lev*)
  (dolist (m (level-monsters *lev*)) (monster-ai m))
  (player-update))

(defun logic ()
  (update-anims)
  (update-monsters *lev*))

(defun quit-game ()
  "Quit the game."
  (sdl:push-quit-event))

;; ------------------------------------------------------- [ MAP GENERATION ]
(defun map-set (x y v &optional map)
  (setf (nth y (nth x (or map (level-map *lev*)))) v))

(defun make-tunnel (x1 y1 x2 y2)
  (unless (almost-outside-map-p x1 y1)
    (map-set x1 y1 'ground)
    (cond ((< x1 x2) (make-tunnel (1+ x1) y1 x2 y2))
          ((> x1 x2) (make-tunnel (1- x1) y1 x2 y2))
          ((< y1 y2) (make-tunnel x1 (1+ y1) x2 y2))
          ((> y1 y2) (make-tunnel x1 (1- y1) x2 y2)))))

(defun make-room (x y size)
  (mapc
   (lambda (p)
     (let ((x* (+ x (car p)))
           (y* (+ y (cdr p))))
       (unless (almost-outside-map-p x* y*)
         (map-set x* y* 'ground))))
   (let ((size-lim (truncate size 2)))
     (loop for i from (- size-lim) to size-lim append
       (loop for j from (- size-lim) to size-lim
             collect (cons i j)))))

  (lambda (&optional p r)
    (cond
      ((eq p 'connect)
       (let ((target (funcall r)))
         (make-tunnel x y (car target) (cdr target))))
      (t (cons x y)))))

(defun walls-within (map x y range)
  (length
   (remove-if-not
    (lambda (s) (or (eq s 'wall) (not s)))
    (loop for i from (- range) to range append
      (loop for j from (- range) to range
            collect (map-ref (+ x i) (+ y j) map))))))

(defun map-disjoint-p (map)
  (let ((map (copy-tree map))
        (pos (random-free-square map)))

    (defun flood-fill (x y)
      (when (and (not (outside-map-p x y))
                 (walkable-p (map-ref x y map)))
        (map-set x y 'fill map)
        (flood-fill (1+ x) y)      (flood-fill (1- x) y)
        (flood-fill x (1+ y))      (flood-fill x (1- y))
        (flood-fill (1+ x) (1+ y)) (flood-fill (1+ x) (1- y))
        (flood-fill (1- x) (1- y)) (flood-fill (1- x) (1+ y))))

    (or (not pos)
        (progn (flood-fill (car pos) (cdr pos))
               (map-has-free-square map)))))

(defun map-gen-rooms (depth)
  (defun iter (level x1 y1 x2 y2)
    (let ((orient (if (zerop (random 2)) 'hor 'vert)))
      (cond ((= level depth)
             (make-room (avg x1 x2) (avg y1 y2) (+ 3 (random 5))))
            ((eq orient 'vert)
             (let ((room1 (iter (1+ level) x1 y1 (avg x1 x2) y2))
                   (room2 (iter (1+ level) (avg x1 x2) y1 x2 y2)))
               (funcall room1 'connect room2)
               room1))
            ((eq orient 'hor)
             (let ((room1 (iter (1+ level) x1 y1 x2 (avg y1 y2)))
                   (room2 (iter (1+ level) x1 (avg y1 y2) x2 y2)))
               (funcall room1 'connect room2)
               room1)))))
  (iter 0 0 0 *width* *height*))

(defun map-gen-cel (depth)
  (defun cel-iter (prev-map i)
    (if (zerop i)
        prev-map
        (let ((mapz
               (loop for x from 0 to (1- *width*) collect
                 (loop for y from 0 to (1- *height*) collect
                   (if (or (>= (walls-within prev-map x y 1) 5)
                           (zerop (walls-within prev-map x y 2)))
                       'wall
                       'ground)))))
          (cel-iter mapz (1- i)))))
  (let ((map
         (loop for _ from 1 to *width* collect
           (loop for _ from 1 to *height* collect
             (if (< 40 (random 100))
                 'ground
                 'wall)))))
    (cel-iter map depth)))

(defun map-gen-all-walls ()
  (loop for _ from 1 to *width* collect
    (loop for _ from 1 to *height* collect 'wall)))

;; -------------------------------------------------------------- [ SPRITES ]
(defparameter *sprites* nil
  "List of loaded sprites.")

(defun load-sprites (sprite-root-dir)
  "Load every sprite located in the *SPRITE-SUBDIRS* directories."
  (let ((sprite-subdirs
         '(("eq" :alpha)
           ("item" :alpha)
           ("monster" :alpha)
           ("obj" :alpha)
           ("tile" :dark)
           ("ui"))))
    (loop for (dir . options) in sprite-subdirs do
      (let ((file-paths (list-directory (strcat sprite-root-dir dir))))
        (dolist (file-path file-paths)
          (load-sprite
           file-path
           :alpha (find :alpha options)
           :dark (find :dark options)))))))

(defun load-sprite (file-path &key alpha dark)
  "Load sprite from file found at FILE-PATH into *SPRITES*. When ALPHA is T,
respect the sprite's alpha channel. When DARK is T, create an additional
dark version of the sprite."
  (let ((surface (sdl:load-image file-path :alpha (and alpha 255)))
        (sprite-name
         (symbolicate
          (string-upcase (first (last (pathname-directory file-path))))
          "-"
          (string-upcase (pathname-name file-path)))))
    (push (cons sprite-name surface) *sprites*)
    (when dark
      (let ((dark-surface (sdl:copy-surface :surface surface))
            (overlay (sdl:create-surface *tile-size* *tile-size* :alpha 128)))
        (sdl:blit-surface overlay dark-surface)
        (push (cons (symbolicate 'dark- sprite-name) dark-surface)
              *sprites*)))))

(defun sprite (name &key dark)
  "Return the SDL-surface of NAME'd sprite. When DARK is T, return the dark
version of NAME'd sprite."
  (let ((sprite (assoc (if dark (symbolicate 'dark- name) name) *sprites*)))
    (if sprite
        (cdr sprite)
        (error "SPRITE: Tried to use unloaded sprite: ~a" name))))

;; ------------------------------------------------------------------ [ FOV ]
(defun recompute-fov ()
  (setf *fov*
        (remove-duplicates
         (loop for i from 0 to (* 2 pi) by 0.01 append
           (dispatch (player-x *p1*) (player-y *p1*) 6 i))
         :test #'equal))
  (dolist (pos *fov*)
    (pushnew pos (level-discovery *lev*) :test #'equal)))

(defun dispatch (sx sy length angle)
  (defun iter (~x ~y res i)
    (let* ((x (round ~x))
           (y (round ~y))
           (next (cons x y)))
      (if (or (= i length)
              (outside-map-p x y)
              (fov-blocking-p (map-ref x y)))
          (cons next res)
          (iter (+ ~x (cos angle)) (+ ~y (sin angle))
                (cons next res) (1+ i)))))
  (iter sx sy nil 0))

;; ------------------------------------------------------------- [ MESSAGES ]
(defparameter *messages* '("Welcome, adventurer!"))

(defun display-messages (rx n)
  (loop for msg in *messages*
        for i from 0 to (1- n)
        do (sdl:draw-string-blended-*
            msg (+ rx 10) (- *win-h* (+ 20 (* i 20))))))

(defun add-message (msg)
  (push (string-capitalize msg :end 1) *messages*))

;; -------------------------------------------------------------- [ DISPLAY ]
(defun draw-panel ()
  (let ((rx (- *win-w* *panel-w*)))
    (sdl:draw-box
     (sdl:rectangle :x rx :y 0 :w *panel-w* :h *win-h*)
     :color (sdl:color :r 20 :g 20 :b 20))

    (sdl:draw-box
     (sdl:rectangle :x (+ rx 10) :y 35 :w (- *panel-w* 20) :h 32)
     :color sdl:*red*)

    (sdl:draw-string-blended-*
     (format nil "HP: ~a/~a" (player-hp *p1*)
             (player-max-hp *p1*))
     (+ rx 10) 10)

    (sdl:draw-box
     (sdl:rectangle
      :x (+ rx 10)
      :y 35
      :w (round (* (/ (player-hp *p1*) (player-max-hp *p1*))
                   (- *panel-w* 20)))
      :h 32)
     :color sdl:*green*)

    (sdl:draw-string-blended-*
     (format nil "Dungeon: ~a" (level-name *lev*))
     (+ rx 10) 80)

    (sdl:draw-string-blended-*
     (format nil "Level: ~a" (player-lvl *p1*))
     (+ rx 10) 100)

    (sdl:draw-string-blended-*
     (format nil "Weapon: ~a"
             (if (player-weapon *p1*)
                 (object-name (player-weapon *p1*))
                 "fists"))
     (+ rx 10) 120)

    (sdl:draw-string-blended-*
     (format nil "Armor: ~a"
             (if (player-armor *p1*)
                 (object-name (player-armor *p1*))
                 "none"))
     (+ rx 10) 140)

    (sdl:draw-string-blended-*
     (format nil "Helmet: ~a"
             (if (player-helmet *p1*)
                 (object-name (player-helmet *p1*))
                 "none"))
     (+ rx 10) 160)

    ;; Urk... Do something about these three. ↓ ↓ ↓
    ;; A lot of magic numbers.

    ;; Item frames
    (loop for i from 0 to (1- *bag-size*) do
      (let ((x (+ rx 10 (* (mod i 8) *tile-size*)))
            (y (+ 200 (* *tile-size* (truncate i 8)))))
        (sdl:draw-surface-at (sprite 'ui-frame)
                             (sdl:point :x x :y y))))

    ;; Actual items
    (loop for item in (player-bag *p1*)
          for i from 0 do
      (let ((x (+ rx 10 (* (mod i 8) *tile-size*)))
            (y (+ 200 (* *tile-size* (truncate i 8)))))
        (when (equipped-p item)
          (sdl:draw-surface-at (sprite 'ui-frame-hi) (sdl:point :x x :y y)))
        (sdl:draw-surface-at (item-sprite item) (sdl:point :x x :y y))))

    ;; Numbered layer
    (when *grab-next-key*
      (loop for item in (player-bag *p1*)
            for i from 0 do
        (let ((x (+ rx 14 (* (mod i 8) *tile-size*)))
              (y (+ 202 (* *tile-size* (truncate i 8)))))
          (sdl:draw-surface-at (sdl:render-string-shaded
                                (string (aref *key-item-map* i))
                                sdl:*white* sdl:*black*)
                               (sdl:point :x x :y y)))))

    (display-messages 0 5)))

(defun draw (spr x y)
  "Draw sprite SPR with relative offsets X and Y wrt. the player. Sprites
too wide will be centered. Sprites too high will flood on top."
  (let ((pix-dif-x (- (sdl:width spr) *tile-size*))
        (pix-dif-y (- (sdl:height spr) *tile-size*)))
    (sdl:draw-surface-at
     spr
     (sdl:point :x (- (* (+ *vp-x* x) *tile-size*) (truncate pix-dif-x 2))
                :y (- (* (+ *vp-y* y) *tile-size*) pix-dif-y)))))

(defun draw-player ()
  (draw (sprite 'obj-player) 0 0)
  (when (player-weapon *p1*)
    (draw (eq-sprite (player-weapon *p1*)) 0 0))
  (when (player-armor *p1*)
    (draw (eq-sprite (player-armor *p1*)) 0 0))
  (when (player-boots *p1*)
    (draw (eq-sprite (player-boots *p1*)) 0 0))
  (when (player-helmet *p1*)
    (draw (eq-sprite (player-helmet *p1*)) 0 0)))

(defun display ()
  (sdl:clear-display sdl:*black*)

  (let ((px (player-x *p1*))
        (py (player-y *p1*)))

    ;; Draw tiles
    (loop for x from (- *vp-x*) to *vp-x* do
      (loop for y from (- *vp-y*) to *vp-y* do
        (let* ((pos (cons (+ px x) (+ py y)))
               (fov (find pos *fov* :test #'equal))
               (dis (find pos (level-discovery *lev*) :test #'equal)))
          (when (or fov dis)
            (let ((tile (or (map-ref (+ px x) (+ py y)) 'wall)))
              (sdl:draw-surface-at-*
               (sprite (symbolicate 'tile- tile) :dark (not fov))
               (* (+ *vp-x* x) *tile-size*)
               (* (+ *vp-y* y) *tile-size*)))))))

    ;; Draw stairs
    (loop for (pos level direction) in (level-stairs *lev*) do
      (let* ((fov (find pos *fov* :test #'equal))
             (dis (find pos (level-discovery *lev*) :test #'equal))
             (sprite
              (sprite (if (eq direction 'up) 'obj-ladder 'obj-stairs))))
        (when (or fov dis)
          (draw sprite (- (car pos) px) (- (cdr pos) py)))))

    ;; Draw items
    (dolist (m (level-items *lev*))
      (when (find (cons (item-x m) (item-y m)) *fov* :test #'equal)
        (draw (item-sprite m) (- (item-x m) px) (- (item-y m) py))))

    ;; Draw player
    (draw-player)

    ;; Draw monsters
    (dolist (m (level-monsters *lev*))
      (when (find (cons (monster-x m) (monster-y m)) *fov* :test #'equal)
        (draw (sprite (type-of m))
              (- (monster-x m) px) (- (monster-y m) py))))

    ;; Draw animation
    (dolist (a (reverse *anims*))
      (setf (sdl:alpha (anim-sprite a)) (anim-alpha a))
      (sdl:draw-surface-at
       (anim-sprite a)
       (sdl:point
        :x (+ (* *tile-size* *vp-x*)
              (* (- (anim-x a) px) *tile-size*)
              (anim-x-off a))
        :y (+ (* *tile-size* *vp-y*)
              (* (- (anim-y a) py) *tile-size*)
              (anim-y-off a))))))

  (draw-panel)

  (sdl:update-display))

;; ---------------------------------------------------------------- [ TILES ]
(defun walkable-p (tile)
  (find tile '(ground stairs)))

(defun monster-walkable-p (tile)
  (find tile '(ground)))

(defun fov-blocking-p (tile)
  (find tile '(wall)))

;; ------------------------------------------------------- [ MAP INSPECTION ]
(defun map-ref (x y &optional map)
  (if (or (< x 0) (< y 0))
      nil
      (nth y (nth x (or map (level-map *lev*))))))

(defun reachable-square-p (x y &optional map)
  (and (not (outside-map-p x y))
       (walkable-p (map-ref x y map))))

(defun free-square-p (x y &optional map)
  (and (reachable-square-p x y map)
       (not (occupied-p x y map))))

(defun occupied-p (x y &optional map)
  (or (object-at 'monster x y map)
      (equal (player-pos *p1*) (cons x y))))

(defun next-to-player-p (x y)
  (and (<= (abs (- x (player-x *p1*))) 1)
       (<= (abs (- y (player-y *p1*))) 1)))

(defun outside-map-p (x y)
  (or (< x 0) (< y 0) (>= x *width*) (>= y *height*)))

(defun almost-outside-map-p (x y)
  (or (outside-map-p x y)
      (< x 1) (< y 1) (>= x (1- *width*)) (>= y (1- *height*))))

(defun map-has-free-square (&optional map)
  (find-if (lambda (s) (walkable-p s))
           (reduce #'append (or map (level-map *lev*)))))

(defun random-free-square (&optional map)
  (defun try ()
    (let ((x (random *width*))
          (y (random *height*)))
      (if (walkable-p (map-ref x y map))
          (cons x y)
          (try))))
  (when (map-has-free-square (or map (level-map *lev*))) (try)))

(defun object-at (type x y &optional lev)
  (find-if (lambda (o) (and (= x (funcall (symbolicate type '-x) o))
                       (= y (funcall (symbolicate type '-y) o))))
           (funcall (symbolicate 'level- type 's) (or lev *lev*))))

;; -------------------------------------------------------------- [ OBJECTS ]
(defun object-name (m)
  (let ((full-name (string (type-of m))))
    (string-downcase (subseq full-name (1+ (search "-" full-name))))))

;; ----------------------------------------------------------- [ LEVEL UTIL ]
(defun change-level (level-num)
  (setf *lev* (nth level-num *levels*)))

(defun add-level (level)
  (push level *levels*))

(defun lev (name)
  (find-if (lambda (l) (eq (level-name l) name)) *levels*))

(defun connect-levels (l1 l2)
  (labels ((recur (m1 m2)
             (let* ((pos (random-free-square m1))
                    (x (car pos))
                    (y (cdr pos)))
               (if (reachable-square-p x y m2)
                   (progn
                     (add-stairs l1 x y l2 'down)
                     (add-stairs l2 x y l1 'up))
                   (recur m1 m2)))))
    (recur (level-map (lev l1)) (level-map (lev l2)))))

;; ----------------------------------------------------- [ MAP MANIPULATION ]
(defun spawn-player ()
  (let ((pos (random-free-square)))
    (setf
     *p1*
     (make-player
      :x      (car pos)
      :y      (cdr pos)
      :hp     50
      :max-hp 50
      :lvl    1
      :xp     0
      :luck   5
      :bag    nil
      :hunger 1100))))

(defun spawn-monster (m level)
  (let ((pos (random-free-square (level-map level))))
    (push (funcall m :x (car pos) :y (cdr pos)) (level-monsters level))))

(defun spawn-item (i level &optional x y)
  (let ((pos (random-free-square (level-map level))))
    (setf (item-x i) (or x (car pos)))
    (setf (item-y i) (or y (cdr pos)))
    (push i (level-items level))))

(defun add-stairs (l1 x y l2 direction)
  (let ((stair-list (list (cons x y) l2 direction)))
    (push stair-list (level-stairs (lev l1)))))

;; --------------------------------------------------------- [ KEY HANDLING ]
(defparameter *key-item-map*
  '#(#\1 #\2 #\3 #\4 #\5 #\6 #\7 #\8
     #\Q #\W #\E #\R #\T #\Y #\U #\I
     #\A #\S #\D #\F #\G #\H #\J #\K
     #\Z #\X #\C #\V #\B #\N #\M #\,))

(defun key-to-item (key)
  "Return item number corresponding to KEY (from *KEY-ITEM-MAP* order)."
  (if (eq key :SDL-KEY-COMMA)
      30 ; special case for comma, since "," ≠ "COMMA"
      (position (string (elt (string key) 8))
                *key-item-map*
                :test #'string=)))

(defparameter *key-map*
  '(((:SDL-KEY-ESCAPE :SDL-KEY-Q) . quit-game)
    ((:SDL-KEY-LEFT   :SDL-KEY-H) . player-move-l)
    ((:SDL-KEY-DOWN   :SDL-KEY-J) . player-move-d)
    ((:SDL-KEY-UP     :SDL-KEY-K) . player-move-u)
    ((:SDL-KEY-RIGHT  :SDL-KEY-L) . player-move-r)
    ((:SDL-KEY-Y)                 . player-move-ul)
    ((:SDL-KEY-U)                 . player-move-ur)
    ((:SDL-KEY-B)                 . player-move-dl)
    ((:SDL-KEY-N)                 . player-move-dr)
    ((:SDL-KEY-G)                 . player-grab)
    ((:SDL-KEY-D)                 . player-drop)
    ((:SDL-KEY-A)                 . player-apply)
    ((:SDL-KEY-I)                 . player-inspect)
    ((:SDL-KEY-PERIOD)            . player-rest)
    ((:SDL-KEY-RETURN)            . player-climb)))

(defun key-handle (key)
  (cond
    (*grab-next-key*
     (progn (funcall *next-item-function* (key-to-item key))
            (setf *grab-next-key* nil)))
    ((zerop (player-hp *p1*))
     (case key
       ((:SDL-KEY-ESCAPE :SDL-KEY-Q) (sdl:push-quit-event))))
    (t
     (let ((key-fun (assoc key *key-map* :test #'find)))
       (when key-fun (funcall (cdr key-fun)))))))

;; ---------------------------------------------------------------- [ FONTS ]
(defparameter *font-large* nil
  "Large version of the default font.")

(defun init-fonts ()
  (let ((default-font-file "fonts/DidactGothic.ttf")
        (default-font-size 16)
        (default-font-size-large 20))

    (sdl:initialise-default-font
     (make-instance
      'sdl:ttf-font-definition
      :size default-font-size
      :filename default-font-file))

    (setf
     *font-large*
     (sdl:initialise-font
      (make-instance
       'sdl:ttf-font-definition
       :size default-font-size-large
       :filename default-font-file)))))

;; ----------------------------------------------------------------- [ MAIN ]
(sdl:with-init ()
  (init-fonts)
  (sdl:window *win-w* *win-h* :title-caption "dog")

  (sdl:enable-key-repeat nil nil)
  (setf (sdl:frame-rate) 30)
  (load-sprites "sprites/")

  (let ((l (make-level :name 'first :map (map-gen-all-walls))))
    (add-level l)
    (loop until (not (map-disjoint-p (level-map l)))
          do (setf (level-map l) (map-gen-cel 4)))
    (spawn-monster #'make-monster-demon l)

    (dotimes (_ 8) (spawn-monster #'make-monster-rat l))
    (dotimes (_ 5)
      (spawn-item (make-instance 'item-potion :color 'red :size 'big) l))

    (dotimes (_ 1) (spawn-item (make-instance 'item-banana) l))
    (dotimes (_ 1) (spawn-item (make-instance 'item-radish) l))
    (dotimes (_ 1) (spawn-item (make-instance 'item-pear) l))
    (dotimes (_ 3) (spawn-item (make-instance 'item-short-sword) l))
    (dotimes (_ 1) (spawn-item (make-instance 'item-long-sword) l))
    (dotimes (_ 3) (spawn-item (make-instance 'item-leather-armor) l))
    (dotimes (_ 3) (spawn-item (make-instance 'item-leather-boots) l))
    (dotimes (_ 1) (spawn-item (make-instance 'item-steel-armor) l))
    (dotimes (_ 3) (spawn-item (make-instance 'item-steel-helmet) l)))

  (let ((l (make-level :name 'second :map (map-gen-all-walls))))
    (add-level l)
    (loop until (not (map-disjoint-p (level-map l)))
          do (setf (level-map l) (map-gen-cel 4)))
    (dotimes (_ 5) (spawn-monster #'make-monster-rat l))
    (dotimes (_ 4) (spawn-monster #'make-monster-demon l)))

  (let ((l (make-level :name 'last :map (map-gen-all-walls))))
    (add-level l)
    (loop until (not (map-disjoint-p (level-map l)))
          do (setf (level-map l) (map-gen-cel 4)))
    (dotimes (_ 50) (spawn-monster #'make-monster-rat l))
    (spawn-monster #'make-monster-devil l))

  (connect-levels 'first 'second)
  (connect-levels 'second 'last)

  (setf *lev* (lev 'first))
  (spawn-player)
  (recompute-fov)

  (sdl:with-events ()
    (:quit-event () t)
    (:key-down-event (:key key) (key-handle key))
    (:video-expose-event () (sdl:update-display))
    (:idle () (logic) (display))))
