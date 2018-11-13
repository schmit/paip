(defun last-name (name)
  (first (last name)))

(defun first-name (name)
  (if (member (first name) *titles*)
      (first-name (rest name))
      (first name)))

(setf names '((John Q Public) (Malcolm X) (Admiral Grace Murray Hopper) (Spot) (Aristotle) (A A Milne)
              (Z Z Top) (Sir Larry Olivier) (Miss Scarlett)))

(defparameter *titles*
  '(Mr Mrs Miss Ms Sir Madam Dr Admiral Major General)
  "A list of titles that can appear at the start of a name.")

(defun mappend (fn the-list)
  "Apply fn to each element of the list and append the results."
  (apply #'append (mapcar fn the-list)))

(defun self-and-double (x) (list x (+ x x)))


(defun numbers-and-negations (input)
  (mappend #'number-negation input))

(defun number-negation (x)
  (if (numberp x)
      (list x (- x))
      nil))
