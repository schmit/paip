(defparameter *simple-grammar*
  '((sentence -> (noun-phrase verb-phrase))
    (noun-phrase -> (Article Noun))
    (verb-phrase -> (Verb noun-phrase))
    (Article -> the a)
    (Noun -> man ball woman table)
    (Verb -> hit saw took liked))
  "A grammar for a trivial subset of English")

(defvar *grammar* *simple-grammar*)

(defun rule-lhs (rule)
  "The left hand side of a rule."
  (first rule))

(defun rule-rhs (rule)
  "The right hand side of a rule."
  (rest (rest rule)))

(defun rewrites (category)
  "Return a list of possible rewrites for this category."
  (rule-rhs (assoc category *grammar*)))

(defun generate (phrase)
  "Generate a random sentence or phrase."
  (cond (;; if a list, generate each element and append
         (listp phrase)
         (mappend #'generate phrase))
        (;; select a random option from rewrites rules
         (rewrites phrase)
         (generate (random-elt (rewrites phrase))))
        (;; otherwise, must be a word; return list with word
         t (list phrase))))

;; 2.5
(defparameter *bigger-grammar*
  '((sentence -> (noun-phrase verb-phrase))
    (noun-phrase -> (Article Adj* Noun PP*) (Name) (Pronoun))
    (verb-phrase -> (Verb noun-phrase PP*))
    (PP* -> () (PP PP*))
    (Adj* -> () (Adj Adj*))
    (PP -> (Prep noun-phrase))
    (Prep -> to in by with on)
    (Adj -> (Size) (Color) adiabatic)
    (Size -> big small tiny huge)
    (Color -> red blue green yellow white black purple)
    (Article -> the a)
    (Name -> Pat Kim Lee Terry Robin)
    (Noun -> man ball woman table)
    (Verb -> hit took saw liked)
    (Pronoun -> he she it these those that)))

(setf *grammar* *bigger-grammar*)


;; 2.6

(defun generate-tree (phrase)
  "Generate a random sentence or phrase."
  (cond (;; if a list, generate each element and append
         (listp phrase)
         (mapcar #'generate-tree phrase))
        (;; select a random option from rewrites rules
         (rewrites phrase)
         (cons phrase
               (generate-tree (random-elt (rewrites phrase)))))
        (;; otherwise, must be a word; return list with word
         t (list phrase))))


(defun combine-all (xlist ylist)
  "Return a list of lists formed by appending a y to an x.
   That is, (combine-all '((a) (b)) '((1) (2)))
   -> ((A 1) (B 1) (A 2) (B 2))"
  (mappend #'(lambda (y)
               (mapcar #'(lambda (x) (append x y)) xlist))
           ylist))


;; Recycled functions
(defun random-elt (choices)
  " Choose an element from a list at random "
  ;; (elt list index) returns the element at index from list
  ;; (random N) generates a random number between 0 and N-1
  (elt choices (random (length choices))))

(defun mappend (fn the-list)
  "Apply fn to each element of the list and append the results."
  (apply #'append (mapcar fn the-list)))
