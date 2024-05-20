(in-package posix)

(defun assert-fail (assertion file line func)
   (declare (external "__assert_fail"))
   (fputs assertion *upc-standard-error*)
   (fputs file *upc-standard-error*)
   (fputs line *upc-standard-error*)
   (fputs func *upc-standard-error*))
