(in-package posix)

(require user)

(defun uids-getgrnam (name)
  (declare (external "getgrnam"))
  (uids-getuser-struct name))

(defun uids-initgroups (name gid)
  (declare (external "initgroups"))
  0)


