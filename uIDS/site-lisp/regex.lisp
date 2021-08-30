(in-package posix)

(defun uids-regcomp (preg regex cflags)
  (declare (external "regcomp"))
  0)

(defun uids-regexec (preg str nm pm eflags)
  (declare (external "regexec"))
  0)
