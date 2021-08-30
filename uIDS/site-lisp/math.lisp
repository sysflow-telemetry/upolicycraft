(in-package posix)

(defun sqrt (x)
  (declare (external "sqrt"))
  (uids-ocaml-sqrt x))

(defun log (x)
  (declare (external "log"))
  (uids-ocaml-log x))

(defun floor (x)
  (declare (external "floor"))
  (uids-ocaml-floor x))

(defun round (x)
  (declare (external "round"))
  (uids-ocaml-round x))

