(defparameter *tutorials*
  `((grabbing
     :limit 3
     :message-string "Press ~a to grab item"
     :format-arguments ,(action-to-string 'player-grab))
    (climbing
     :limit 2
     :message-string "Press ~a to climb"
     :format-arguments ,(action-to-string 'player-climb))))

(defun tutor (topic)
  "Add a tutoring overlay regarding TOPIC, unless the limit for this TOPIC
is reached."
  (let ((tutorial (cdr (assoc topic *tutorials*))))
    (when (< 0 (getf tutorial :limit))
      (decf (getf tutorial :limit))
      (add-overlay-message
       (getf tutorial :message-string)
       (getf tutorial :format-arguments)))))
