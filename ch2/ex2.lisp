;; Ex 1 Rewrite generate with cond but without using rewrites phrase twice

(defun generate (phrase)
  "Generate a random sentence or phrase."
  (let ((choices nil))
      (cond (;; if a list, generate each element and append
             (listp phrase)
             (mappend #'generate phrase))
            (;; select a random option from rewrites rules
             (setf choices (rewrites phrase))
             (generate (random-elt choices)))
            (;; otherwise, must be a word; return list with word
             t (list phrase)))))


;; Ex 2 Write a version of generate that explicitly differentiates
;; between terminal symbols and nonterminal symbols

(defun generate (phrase)
  "Generate a random sentence or phrase."
  (cond (;; if a list, generate each element and append
         (listp phrase)
         (mappend #'generate phrase))
        (;; select a random option from rewrites rules
         (non-terminal-p phrase)
         (generate (random-elt (rewrites phrase))))
        (;; otherwise, must be a word; return list with word
         t (list phrase))))

(defun non-terminal-p (category)
  "Returns true if it has rewrites rules"
  (not (null (rewrites category))))

;; Ex 4 Generalize combine-all to cross-product and define
;; combine-all in terms of cross-product

(defun cross-product (fn xlist ylist)
  (mappend #'(lambda (y)
               (mapcar #'(lambda (x) (funcall fn x y)) xlist))
           ylist))

(defun combine-all (xlist ylist)
  (cross-product #'append xlist ylist)
