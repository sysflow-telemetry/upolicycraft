(in-package posix)

(defun uids-setrlimit (resource rlim)
  (declare (external "setrlimit64"))
  0)
