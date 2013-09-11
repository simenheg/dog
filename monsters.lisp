(defstruct monster x y hp atck xp (unique nil))

(defstruct (monster-rat
             (:include monster
                       (hp (random-range 2 5))
                       (atck 1)
                       (xp 1))))

(defstruct (monster-demon
             (:include monster
                       (hp (random-range 15 20))
                       (atck 5)
                       (xp 5))))

(defstruct (monster-devil
             (:include monster
                       (hp 666)
                       (atck 666)
                       (xp 666))))

(defun monster-move (m x y)
  (when (and (free-square-p x y)
             (monster-walkable-p (map-ref x y)))
    (setf (monster-x m) x)
    (setf (monster-y m) y)))

(defmethod monster-attack (m)
  (let ((atck (monster-atck m))
        (pos  (player-pos *p1*)))
    (add-message (format nil "~a hits you for ~a damage!"
                         (monster-print-name m) atck))
    (setf (player-hp *p1*) (max 0 (- (player-hp *p1*) atck)))
    (make-damage-anim (car pos) (cdr pos) (format nil "~a" atck))))

(defmethod monster-print-name (m)
  (format nil "~a~a" (if (monster-unique m) "" "the ")
          (string-capitalize (object-name m))))

(defmethod monster-die (m)
  (add-message (format nil "~a dies!" (monster-print-name m)))
  (spawn-item (make-instance 'item-corpse) *lev* (monster-x m) (monster-y m))
  (xp-gain (monster-xp m)))

(defmethod monster-ai (m)
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

(defmethod monster-pix-pos (m)
  (sdl:point :x (* (monster-x m) *tile-size*)
             :y (* (monster-y m) *tile-size*)))

(defmethod monster-show (m)
  (format t "You see a ~a" (type-of m)))

(defun update-monsters (level)
  (setf (level-monsters level)
        (mapcan (lambda (m) (if (<= (monster-hp m) 0)
                           (progn (monster-die m) nil)
                           (list m)))
                (level-monsters level))))
