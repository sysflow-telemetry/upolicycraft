(in-package posix)

(defun assert-fail (assertion file line func)
   (declare (external "__assert_fail"))
   (fputs assertion *uids-standard-error*)
   (fputs file *uids-standard-error*)
   (fputs line *uids-standard-error*)
   (fputs func *uids-standard-error*))
