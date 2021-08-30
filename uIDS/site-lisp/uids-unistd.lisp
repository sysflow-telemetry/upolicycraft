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

(defun uids-fstat (fd buf)
  (declare (external "fstat64" "__fstat"))
  (let ((offset 48))
    ;; (write-word ptr_t (+ buf offset) size)
    (uids-ocaml-fstat fd buf)))

(defun uids-stat (filename buf)
  (declare (external "stat64" "__stat"))
  ;;(uids-ocaml-debug 0xf0bc0de)
  (let ((offset 48))
    ;; (write-word ptr_t (+ buf offset) size)
    (uids-ocaml-stat filename buf)))

(defun uids-xstat (vers path buf)
  (declare (external "__xstat"))
  (let ((fd (uids-channel-open path 0)))
    (if (= fd -1)
      -1
      (let ()
        (uids-channel-close fd)
        0))))

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
