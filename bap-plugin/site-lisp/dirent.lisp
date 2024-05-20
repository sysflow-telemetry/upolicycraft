(in-package posix)

(defun upc-opendir (path)
  (declare (external "opendir"))
  (upc-ocaml-opendir path))

(defparameter dirent (malloc 512))

(defun upc-readdir (dir)
  (declare (external "readdir"))
  (upc-ocaml-readdir dir dirent))
