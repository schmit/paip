(defvar *dbg-ids* nil "Identifiers used by dbg")

(defun dbg (id format-string &rest args)
  "Print debugging info if (DEBUG ID) has been specified"
  (when (member id *dbg-ids*)
    (fresh-line *debug-io*)
    (apply #'format-string *debug-io* format-string args)))

(defun debug (&rest ids)
  "Start dbg output on the given ids"
  (setf *dbg-ids* (union ids *dbg-ids*)))

(defun undebug (&rest ids)
  "Stop dbg on the list of ids. With no ids, stop dbg altogether"
  (setf *dbg-ids*
        (if (null ids) nil
            (set-difference *dbg-ids* ids))))
