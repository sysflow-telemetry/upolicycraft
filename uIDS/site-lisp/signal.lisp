(in-package posix)

(defun uids-sigfillset (st)
  (declare (external "sigfillset"))
  0)

(defun uids-sigaction (sm act oldact)
  (declare (external "sigaction"))
  0)


