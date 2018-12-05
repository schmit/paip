
(defun GPS (state goals &optional (*ops* *ops*))
  "General problem solver: achieve all goals using *ops*"
  (remove-if #'atom (achieve-all (cons '(start) state) goals nil)))

(defun use (oplist)
  "Use oplist as the default list of operators."
  (length (setf *ops* oplist)))

(defvar *ops* nil "A list of available operators")

(defstruct op "An operation"
           (action nil) (preconds nil) (add-list nil) (del-list nil))

(defun achieve-all (state goals goal-stack)
  "Try to achieve each goal, then make sure they still hold."
  (let ((current-state state))
    (if (and (every #'(lambda (g)
                        (setf current-state
                              (achieve current-state g goal-stack)))
                    goals)
             (subsetp goals current-state :test #'equal))
        current-state)))

(defun achieve (state goal goal-stack)
  "A goal is achieved if it already holds or if there is
  an appropriate op for it that is applicable."
  (cond ((member-equal goal state) state)
        ((member-equal goal goal-stack) nil)
        (t (some #'(lambda (op) (apply-op state goal op goal-stack))
                  (find-all goal *ops* :test #'appropriate-p)))))

(defun member-equal (item list)
  (member item list :test #'equal))

(defun appropriate-p (goal op)
  "An op is appropriate to a goal if it is in its add list"
  (member-equal goal (op-add-list op)))

(defun apply-op (state goal op goal-stack)
  "Return a new, transformed state if op is applicable."
  (let ((new-state (achieve-all state (op-preconds op)
                             (cons goal goal-stack))))
    (unless (null new-state)
      ;; Return an updated state
      (append (remove-if #'(lambda (x)
                             (member-equal x (op-del-list op)))
                         new-state)
              (op-add-list op)))))

(defun executing-p (x)
  "Is x ofr the form (executing ... ) ?"
  (starts-with x 'executing))

(defun starts-with (list x)
  "Is this a list whose first element is x?"
  (and (consp list) (eql (first list) x)))

(defun convert-op (op)
  "Make op conform to the (EXECUTING op) convention"
  (unless (some #'executing-p (op-add-list op))
    (push (list 'executing (op-action op)) (op-add-list op)))
  op)

(defun op (action &key preconds add-list del-list)
  "Make a new operator that obeys the (EXECUTING op) convention"
  (convert-op
   (make-op :action action :preconds preconds
            :add-list add-list :del-list del-list)))


(defun find-all (item sequence &rest keyword-args
                 &key (test #'eql) test-not &allow-other-keys)
  "Find all those elements of a sequence that match item
  according to the keywords. Does not alter sequence"
  (if test-not
      (apply #'remove item sequence
             :test-not (complement test-not) keyword-args)
      (apply #'remove item sequence
             :test (complement test) keyword-args)))
