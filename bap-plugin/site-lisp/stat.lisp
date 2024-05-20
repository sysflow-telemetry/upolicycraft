(in-package posix)

(defun upc-fstat (fd buf)
  (declare (external "fstat64" "__fstat"))
  (let ((offset 48))
    ;; (write-word ptr_t (+ buf offset) size)
    (upc-ocaml-fstat fd buf)))

(defun upc-stat (filename buf)
  (declare (external "stat64" "__stat"))
  (let ((offset 48))
    ;; (write-word ptr_t (+ buf offset) size)
    (upc-ocaml-stat filename buf)))

(defun upc-xstat (vers path buf)
  (declare (external "__xstat"))
  (let ((fd (upc-channel-open path 0)))
    (if (= fd -1)
      -1
      (let ()
        (upc-channel-close fd)
        0))))

(defun upc-fchmod (fd mode)
  (declare (external "fchmod"))
  0)

(defun upc-mkdir (path mode)
  (declare (external "mkdir"))
  0)
