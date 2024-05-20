(in-package posix)

(defun sqrt (x)
  (declare (external "sqrt"))
  (upc-ocaml-sqrt x))

(defun log (x)
  (declare (external "log"))
  (upc-ocaml-log x))

(defun floor (x)
  (declare (external "floor"))
  (upc-ocaml-floor x))

(defun round (x)
  (declare (external "round"))
  (upc-ocaml-round x))

