(in-package posix)

(require user)

(defun uids-getpwnam (login)
  (declare (external "getpwnam"))
  (uids-getuser-struct login))
