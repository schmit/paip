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


;; Ex 2 Write a function to exponentiate

(defun power (x n)
  " Computes x^n, requires n >= 0, in log(n)"
  (cond ((= n 0) 1)
        ((evenp n) (power (* x x) (/ n 2)))
        (t (* x (power x (- n 1))))))

;; Ex 3 Write a function that counts the number of atoms in an experssion

(defun count-atoms (expr)
  (cond ((null expr) 0)
        ((atom expr) 1)
        (t (+ (count-atoms (first expr)) (count-atoms (rest expr))))))

;; Ex 4 Write a function that counts the number of times an expression occurs
;; anywhere within another expression

(defun count-anywhere (item tree)
  (cond ((eql item tree) 1)
        ((atom tree) 0)
        (t (+ (count-anywhere item (first tree))
              (count-anywhere item (rest tree))))))

;; Ex 5 Write a function to compute the dot product between two sequences
(defun dot-product (x y)
  (if (or (null x) (null y))
      0
      (+ (* (first x) (first y))
         (dot-product (rest x) (rest y))))))
