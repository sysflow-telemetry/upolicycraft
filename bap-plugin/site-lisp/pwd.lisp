(in-package posix)

(require user)

(defun upc-getpwnam (login)
  (declare (external "getpwnam"))
  (upc-getuser-struct login))
