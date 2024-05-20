(in-package posix)

(defparameter *access-used* 2
  "has access been called")

(defun upc-access (path mode)
  (declare (external "access"))
    (if (= *access-used* 0)
      0
      (let ()
        (decr *access-used*)
        -1)))

(defun upc-fork ()
  (declare (external "fork"))
  0)

(defun upc-getuid ()
  (declare (external "getuid"))
  (upc-ocaml-getuid))

(defun upc-chdir (name)
  (declare (external "chdir"))
  0)

(defun upc-fchown (fd owner group)
  (declare (external "fchown"))
  0)

(defparameter optind 0)

(defun upc-getopt (argc argv optstring)
  (declare (external "getopt"))
  (let ((res -1))
    (while (and (< optind argc) (= res -1))
      (let ((target (ptr+ ptr_t argv optind))
            (s (read-word ptr_t target)))
        (if (and (= (memory-read s) ?-)
                 (strstr optstring (ptr+1 char s)))
          (set res (memory-read (ptr+1 char s)))
          0)
    (incr optind)))
    res))

(defun upc-setuid (uid)
  (declare (external "setuid"))
  0)

(defun upc-setgid (gid)
  (declare (external "setgid"))
  0)

(defun upc-select (nfds readfds writefds exceptfds timeout)
  (declare (external "select"))
  1)
