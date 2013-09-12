(defun avg (x &rest xn)
  "Return the mean of the numbers X .. XN, rounded down to nearest integer."
  (floor (funcall #'mean (cons x xn))))

(defun fmt (control-string &rest format-arguments)
  "Same as (format nil control-string format-arguments)."
  (apply #'format (cons nil (cons control-string format-arguments))))

(defun random-range (from to)
  "Return a random number between FROM and TO (inclusive)."
  (+ from (random (1+ (- to from)))))

(defun strcat (&rest strings)
  "Return a new string of all the STRINGS concatenated together."
  (apply #'concatenate 'string strings))
