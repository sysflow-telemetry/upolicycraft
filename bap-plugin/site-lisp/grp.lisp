(in-package posix)

(require user)

(defun upc-getgrnam (name)
  (declare (external "getgrnam"))
  (upc-getuser-struct name))

(defun upc-initgroups (name gid)
  (declare (external "initgroups"))
  0)
