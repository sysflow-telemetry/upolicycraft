(in-package posix)

(defun upc-regcomp (preg regex cflags)
  (declare (external "regcomp"))
  0)

(defun upc-regexec (preg str nm pm eflags)
  (declare (external "regexec"))
  0)
