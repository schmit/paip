;; Context free phrase-structure grammar

(defun sentence ()
  (append (noun-phrase) (verb-phrase)))

(defun noun-phrase ()
  (append (Article) (Adj*) (Noun) (PP*)))

(defun verb-phrase ()
  (append (Verb) (noun-phrase)))

(defun Article ()
  (one-of '(the a)))

(defun Noun ()
  (one-of '(man ball woman table child house tree Spain Mexico)))

(defun Verb ()
  (one-of '(hit took saw liked threw caught ate heard)))

(defun PP* ()
  (if (= (random 2) 0)
      nil
      (append (PP) (PP*))))

(defun PP ()
  (append (Prep) (noun-phrase)))

(defun Adj* ()
  (if (= (random 2) 0)
      nil
      (append (Adj) (Adj*))))

(defun Adj ()
  (one-of '(big little narrow wide tall short red green white yellow blue)))

(defun Prep ()
  (one-of '(to in by with up without)))

(defun one-of (set)
  " Pick a random element from a set and make it a list "
  (list (random-elt set)))

(defun random-elt (choices)
  " Choose an element from a list at random "
  ;; (elt list index) returns the element at index from list
  ;; (random N) generates a random number between 0 and N-1
  (elt choices (random (length choices))))
