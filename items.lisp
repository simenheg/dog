(defclass item ()
  ((x
    :initarg :x
    :accessor item-x
    :documentation "X position of item on the map.")
   (y
    :initarg :y
    :accessor item-y
    :documentation "Y position of item on the map.")))

(defclass food (item) ())
(defclass item-banana (food) ())
(defclass item-radish (food) ())
(defclass item-pear (food) ())
(defclass item-corpse (food) ())

(defclass item-potion (item)
  ((color
    :initarg :color
    :accessor color)
   (size
    :initarg :size
    :accessor size)))

(defclass equipment (item)
  ((eq-sprite
    :initarg :eq-sprite
    :accessor eq-sprite
    :documentation "Small version of the sprite layered on the player sprite
    when the item is equipped.")))

(defclass weapon (equipment)
  ((damage
    :initarg :damage
    :initform (error "Must supply a weapon's damage.")
    :accessor damage)))

(defclass armor (equipment) ())
(defclass boots (equipment) ())
(defclass helmet (equipment) ())

(defclass item-short-sword (weapon)
  ((damage
    :initform 7)
   (eq-sprite
    :initform (sprite 'eq-short-sword))))

(defclass item-short-sword (weapon)
  ((damage
    :initform 7)
   (eq-sprite
    :initform (sprite 'eq-short-sword))))

(defclass item-long-sword (weapon)
  ((damage
    :initform 13)
   (eq-sprite
    :initform (sprite 'eq-long-sword))))

(defclass item-steel-armor (armor)
  ((eq-sprite
    :initform (sprite 'eq-steel-armor))))

(defclass item-leather-armor (armor)
  ((eq-sprite
    :initform (sprite 'eq-leather-armor))))

(defclass item-leather-boots (boots)
  ((eq-sprite
    :initform (sprite 'eq-leather-boots))))

(defclass item-steel-helmet (helmet)
  ((eq-sprite
    :initform (sprite 'eq-steel-helmet))))

(defmethod item-apply (i)
  (add-message "Can't apply the ~a." (object-name i)))

(defmethod item-apply ((i food))
  (remove-from-bag i)
  (add-message "Yum ... ~a!" (object-name i))
  (incf (player-hunger *p1*) 100)
  (end-turn))

(defmethod item-apply ((i item-potion))
  (remove-from-bag i)
  (add-message "Drank the ~a." (object-name i))
  (setf (player-hp *p1*) (min (player-max-hp *p1*) (+ (player-hp *p1*) 100)))
  (end-turn))

(defmacro equipment-apply (slot i)
  `(if (eq ,slot ,i)
       (progn
         (add-message "Unequipped the ~a." (object-name ,i))
         (setf ,slot nil))
       (progn
         (add-message "Equipped the ~a." (object-name ,i))
         (setf ,slot ,i))))

(defmethod item-apply ((i equipment))
  (let ((type (type-of i)))
    (cond
      ((subtypep type 'weapon)
       (equipment-apply (player-weapon *p1*) i))
      ((subtypep type 'armor)
       (equipment-apply (player-armor *p1*) i))
      ((subtypep type 'boots)
       (equipment-apply (player-boots *p1*) i))
      ((subtypep type 'helmet)
       (equipment-apply (player-helmet *p1*) i))
      (t (error "ITEM-APPLY: Unhandled case: ~a" i))))
  (end-turn))

(defmethod item-inspect (i)
  (add-message "ERROR: Description missing for ~a." (object-name i)))

(defmethod item-inspect ((i food))
  (add-message "A delicious looking ~a." (object-name i)))

(defmethod item-inspect ((i item-potion))
  (add-message "A bubbling ~a." (object-name i)))

(defmethod item-sprite (i)
  (sprite (type-of i)))

(defmethod item-sprite ((i item-potion))
  (sprite
   (symbolicate 'item-potion- (color i) '- (size i))))

(defun bag-full-p ()
  "Return T if the bag is full."
  (= (length (player-bag *p1*)) *bag-size*))

(defun equipped-p (i)
  "Return T if item I is equipped."
  (or (eq (player-weapon *p1*) i)
      (eq (player-armor *p1*) i)
      (eq (player-boots *p1*) i)
      (eq (player-helmet *p1*) i)))

(defun grab-item (item)
  "Move ITEM from the level into the bag, unless the bag is full."
  (cond
    ((bag-full-p)
     (add-message "Bag is full!"))
    (t
     (add-message "Grabbed the ~a." (object-name item))
     (add-to-bag item)
     (setf (level-items *lev*) (remove item (level-items *lev*)))
     (end-turn))))

(defun add-to-bag (item)
  "Add item I to the bag."
  (setf (player-bag *p1*) (append (player-bag *p1*) (list item))))

(defun remove-from-bag (item)
  "Remove ITEM from the bag."
  (setf (player-bag *p1*) (remove item (player-bag *p1*))))

(defun drop-item (n)
  (if (and n (nth n (player-bag *p1*)))
      (let* ((item (nth n (player-bag *p1*))))
        (add-message "Dropped the ~a." (object-name item))
        (remove-from-bag item)
        (setf (item-x item) (player-x *p1*))
        (setf (item-y item) (player-y *p1*))
        (push item (level-items *lev*))
        (end-turn))
      (add-message "No such item.")))

(defun apply-item (n)
  (if (and n (nth n (player-bag *p1*)))
      (item-apply (nth n (player-bag *p1*)))
      (add-message "No such item.")))

(defun inspect-item (n)
  (if (and n (nth n (player-bag *p1*)))
      (item-inspect (nth n (player-bag *p1*)))
      (add-message "No such item.")))
