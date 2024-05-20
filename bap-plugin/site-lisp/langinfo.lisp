(in-package posix)

(defun upc-nl-langinfo (item)
  (declare (external "nl_langinfo"))
  (let ((buf (malloc 64)))
    (memory-write buf 0x41)
    (memory-write (ptr+1 char buf) 0x00)
    buf))
