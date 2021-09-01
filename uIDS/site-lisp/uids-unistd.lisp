(in-package posix)

(defparameter *access-used* 2
  "has access been called")

(defun uids-access (path mode)
  (declare (external "access"))
    (if (= *access-used* 0)
      0
      (let ()
        (decr *access-used*)
        -1)))

(defun uids-fork ()
  (declare (external "fork"))
  0)

(defun uids-getuid ()
  (declare (external "getuid"))
  (uids-ocaml-getuid))

(defun uids-chdir (name)
  (declare (external "chdir"))
  0)

(defun uids-fchown (fd owner group)
  (declare (external "fchown"))
  0)

(defparameter optind 0)

(defun uids-getopt (argc argv optstring)
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

(defun uids-setuid (uid)
  (declare (external "setuid"))
  0)

(defun uids-setgid (gid)
  (declare (external "setgid"))
  0)

(defun uids-select (nfds readfds writefds exceptfds timeout)
  (declare (external "select"))
  1)
