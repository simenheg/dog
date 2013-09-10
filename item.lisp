(defstruct item x y)

(defstruct (food (:include item)))
(defstruct (item-banana (:include food)))
(defstruct (item-radish (:include food)))
(defstruct (item-pear (:include food)))
(defstruct (item-corpse (:include food)))

(defstruct (item-potion (:include item))
  color size)

(defstruct (equipment (:include item))
  eq-sprite)

(defstruct (weapon (:include equipment))
  damage)

(defstruct (armor (:include equipment)))
(defstruct (boots (:include equipment)))
(defstruct (helmet (:include equipment)))

(defstruct (item-short-sword
             (:include weapon
                       (damage 7)
                       (eq-sprite (sprite 'eq-short-sword)))))

(defstruct (item-long-sword
             (:include weapon
                       (damage 13)
                       (eq-sprite (sprite 'eq-long-sword)))))

(defstruct (item-steel-armor
             (:include armor
                       (eq-sprite (sprite 'eq-steel-armor)))))

(defstruct (item-leather-armor
             (:include armor
                       (eq-sprite (sprite 'eq-leather-armor)))))

(defstruct (item-leather-boots
             (:include boots
                       (eq-sprite (sprite 'eq-leather-boots)))))

(defstruct (item-steel-helmet
             (:include helmet
                       (eq-sprite (sprite 'eq-steel-helmet)))))

(defmethod item-apply (i)
  (add-message (format nil "Can't apply the ~a." (object-name i))))

(defmethod item-apply ((i food))
  (remove-from-bag i)
  (add-message (format nil "Yum ... ~a!" (object-name i)))
  (incf (player-hunger *p1*) 100)
  (end-turn))

(defmethod item-apply ((i item-potion))
  (remove-from-bag i)
  (add-message (format nil "Drank the ~a." (object-name i)))
  (setf (player-hp *p1*) (min (player-max-hp *p1*) (+ (player-hp *p1*) 100)))
  (end-turn))

(defmacro equipment-apply (slot i)
  `(if (eq ,slot ,i)
       (progn
         (add-message (format nil "Unequipped the ~a." (object-name ,i)))
         (setf ,slot nil))
       (progn
         (add-message (format nil "Equipped the ~a." (object-name ,i)))
         (setf ,slot ,i))))

(defmethod item-apply ((i equipment))
  (cond
    ((weapon-p i) (equipment-apply (player-weapon *p1*) i))
    ((armor-p i)  (equipment-apply (player-armor *p1*) i))
    ((boots-p i)  (equipment-apply (player-boots *p1*) i))
    ((helmet-p i) (equipment-apply (player-helmet *p1*) i))
    (t (error "ITEM-APPLY: Unhandled case: ~a" i)))
  (end-turn))

(defmethod item-inspect (i)
  (add-message (format nil "ERROR: Description missing for ~a."
                       (object-name i))))

;; (defmethod item-inspect ((i food))
;;   (add-message (format nil "A delicious looking ~a." (object-name i))))

;; (defmethod item-inspect ((i item-potion))
;;   (add-message (format nil "A bubbling ~a." (object-name i))))

(defmethod item-sprite (i)
  (sprite (type-of i)))

(defmethod item-sprite ((i item-potion))
  (sprite
   (symbolicate 'item-potion- (item-potion-color i) '- (item-potion-size i))))
