(defun random-range (from to)
  "Return a random number between FROM and TO (inclusive)."
  (+ from (random (1+ (- to from)))))

(defun strcat (&rest strings)
  "Return a new string of all the STRINGS concatenated together."
  (apply #'concatenate 'string strings))

(defun avg (x &rest xn)
  "Return the mean of the numbers X .. XN, rounded down to nearest integer."
  (floor (funcall #'mean (cons x xn))))
