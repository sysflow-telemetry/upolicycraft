(require simple-memory-allocator)

(in-package posix)

(defun go-new (p size)
  (declare (external "__go_new" "__go_new_nopointers"))
  (malloc size))
