(defclass monster ()
  ((x
    :initarg :x
    :accessor monster-x
    :documentation "X position of monster on the map.")
   (y
    :initarg :y
    :accessor monster-y
    :documentation "Y position of monster on the map.")
   (hp
    :initarg :hp
    :accessor hp
    :documentation "Initial hit points of monster")
   (min-damage
    :initarg :min-damage
    :accessor min-damage
    :documentation "Minimum damage dealt by monster per attack.")
   (max-damage
    :initarg :max-damage
    :accessor max-damage
    :documentation "Maximum damage dealt by monster per attack.")
   (xp
    :initarg :xp
    :accessor xp
    :documentation "Experience points gained from killing monster.")
   (unique-p
    :initarg :unique-p
    :accessor unique-p
    :initform nil
    :documentation "T if monster is unique (max one instance per game).")))

(defclass monster-rat (monster)
  ((hp
    :initform (random-range 2 5))
   (min-damage
    :initform 1)
   (max-damage
    :initform 2)
   (xp
    :initform 1)))

(defclass monster-demon (monster)
  ((hp
    :initform (random-range 15 20))
   (min-damage
    :initform 5)
   (max-damage
    :initform 7)
   (xp
    :initform 5)))

(defclass monster-young-wolf (monster)
  ((hp
    :initform (random-range 25 30))
   (min-damage
    :initform 7)
   (max-damage
    :initform 11)
   (xp
    :initform 7)))

(defclass monster-cerberus (monster)
  ((hp
    :initform (random-range 35 40))
   (min-damage
    :initform 5)
   (max-damage
    :initform 15)
   (xp
    :initform 15)))

(defclass monster-devil (monster)
  ((hp
    :initform (random-range 100 125))
   (min-damage
    :initform 1)
   (max-damage
    :initform 666)
   (xp
    :initform 100)))

(defun monster-move (m x y)
  (when (and (free-square-p x y)
             (monster-walkable-p (map-ref x y)))
    (setf (monster-x m) x)
    (setf (monster-y m) y)))

(defmethod monster-attack ((m monster))
  (let ((dmg (random-range (min-damage m) (max-damage m)))
        (pos (player-pos *p1*)))
    (add-message (format nil "~a hits you for ~a damage!"
                         (monster-print-name m) dmg))
    (setf (player-hp *p1*) (max 0 (- (player-hp *p1*) dmg)))
    (make-damage-anim (car pos) (cdr pos) (format nil "~a" dmg))))

(defmethod monster-print-name ((m monster))
  (format nil "~a~a" (if (unique-p m) "" "the ")
          (string-capitalize (object-name m))))

(defmethod monster-die ((m monster))
  (add-message (format nil "~a dies!" (monster-print-name m)))
  (spawn-item (make-instance 'item-corpse) *lev* (monster-x m) (monster-y m))
  (xp-gain (xp m)))

(defmethod monster-ai ((m monster))
  (let ((px (player-x *p1*))
        (py (player-y *p1*))
        (x  (monster-x m))
        (y  (monster-y m)))
    (if (next-to-player-p x y)
        (monster-attack m)
        (if (zerop (random 2))
            (cond ((< y py) (monster-move m x (1+ y)))
                  ((> y py) (monster-move m x (1- y))))
            (cond ((< x px) (monster-move m (1+ x) y))
                  ((> x px) (monster-move m (1- x) y)))))))

(defmethod monster-pix-pos ((m monster))
  (sdl:point :x (* (monster-x m) *tile-size*)
             :y (* (monster-y m) *tile-size*)))

(defmethod monster-show ((m monster))
  (format t "You see a ~a" (type-of m)))

(defun update-monsters (level)
  (setf (level-monsters level)
        (mapcan
         (lambda (m) (if (<= (hp m) 0)
                    (progn (monster-die m) nil)
                    (list m)))
         (level-monsters level))))
