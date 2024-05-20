(in-package posix)

(defun upc-sigfillset (st)
  (declare (external "sigfillset"))
  0)

(defun upc-sigaction (sm act oldact)
  (declare (external "sigaction"))
  0)


