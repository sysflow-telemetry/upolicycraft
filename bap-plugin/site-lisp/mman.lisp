(require memory-allocator)

(in-package posix)

(defun upc-mmap (addr len prot flags fd offset)
  (declare (external "mmap"))
  (malloc len))

(defun upc-mprotect (addr len prot)
  (declare (external "mprotect"))
  0)

(defun upc-munmap (addr len)
  (declare (external "munmap"))
  0)
