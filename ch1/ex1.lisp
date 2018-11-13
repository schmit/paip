;; Ex 1 Define a version of last-name that handles "Rex Morgan MD",
;; "Morton Downey, Jr." and whatever other cases you can think of

(defparameter *titles*
  '(Mr Mrs Miss Ms Sir Madam Dr Admiral Major General)
  "A list of titles that can appear at the start of a name.")


(defparameter *postfixes*
  '(MD Jr. Sr.)
  "A list of postfixes that can appear at the end of a name.")


(defun first-if (predicate the-list)
  (if (funcall predicate (first the-list))
      (first the-list)
      (first-if predicate (rest the-list))))


(defun first-name (name)
  "Returns the first name, stripping titles"
  (first-if (lambda (s) (not (member s *titles*))) name))

(defun last-name (name)
  "Returns last name, stripping postfixes"
  (first-if (lambda (s) (not (member s *postfixes*))) (reverse name)))
