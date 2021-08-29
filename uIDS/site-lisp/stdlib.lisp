(require string)
(require atoi)
(require stdio)
(require simple-memory-allocator)
(require types)

(in-package posix)

(defun abort ()
  "terminates program with exit code 1"
  (declare (external "abort"))
  (exit-with 1))


(defun exit (code)
  (declare (external "exit" "_exit"))
  (exit-with code))


(defun atexit (cb)
  (declare (external "atexit" "__cxa_atexit"))
  0)

(defun abs (x)
  (declare (external "abs"))
  (if (is-negative x) (neg x) x))
