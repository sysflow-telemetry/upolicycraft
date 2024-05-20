(in-package posix)

(defun upc-setrlimit (resource rlim)
  (declare (external "setrlimit64"))
  0)
