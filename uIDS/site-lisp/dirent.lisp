(in-package posix)

(defun uids-opendir (path)
  (declare (external "opendir"))
  (uids-ocaml-opendir path))

(defparameter dirent (malloc 512))

(defun uids-readdir (dir)
  (declare (external "readdir"))
  (uids-ocaml-readdir dir dirent))
