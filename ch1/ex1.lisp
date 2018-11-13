;; Ex 1 Define a version of last-name that handles "Rex Morgan MD",
;; "Morton Downey, Jr." and whatever other cases you can think of

(defparameter *postfixes*
  '(MD Jr. Sr.))



(defun first-if (predicate the-list)
  (if (funcall predicate (first the-list))
      (first the-list)
      (first-if predicate (rest the-list))))
