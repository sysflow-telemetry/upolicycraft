(require string)
(require ascii)
(require atoi)
(require stdio)
(require simple-memory-allocator)
(require types)

(defparameter *cbloc* nil
  "the starting address of the cbloc-arena")

;; PATH=/usr/bin

(defun getenv (name)
  "finds a value of an environment variable with the given name"
  (declare (external "getenv"))
  (let ((p environ)
        (n (strlen name))
        (r (read-word ptr_t p))
        (result 0))
    (while (and (> (read-word ptr_t p) 0)
                (= result 0))
      (let ((s (read-word ptr_t p))
            (m (cast ptr_t (min n (strlen s)))))
        (uids-ocaml-debug 0xfabc0de)
        (uids-ocaml-debug n)
        (uids-ocaml-debug (strlen s))
        (if (not (memcmp s name m))
             (let ((x 1))
             (uids-ocaml-debug 0xdeadc0de)
             (set result (ptr+1 char (strchr s (cast int ?=)))))
             nil)
        (set p (ptr+1 ptr_t p))
        (uids-ocaml-debug 0xddddd)))
    result))

(defun abort ()
  "terminates program with exit code 1"
  (declare (external "abort"))
  (exit-with 1))

(defun rand ()
  (declare (external "rand"))
  0)

(defun exit (code)
  (declare (external "exit" "_exit"))
  (exit-with code))

(defun atexit (cb)
  (declare (external "atexit"))
  0)

(defun uids-ascii-is-alpha (c)
  (or (and (>= c 0x41) (<= c 0x5a)) (and (>= c 0x61) (<= c 0x7a))))

(defun ctype-b-loc ()
  (declare (external "__ctype_b_loc"))
  (when (not *cbloc*)
    (let ((p (malloc (sizeof ptr_t)))
          (n (+ 129 255))
          (table (malloc (* (sizeof short) n)))
          (start (ptr+ short table 129))
          (i 0))
      (bzero table (* (sizeof short) n))
      (write-word ptr_t p start)
      (while (< i n)
        (let ((q (ptr+ short table i)))
          (when (and (>= i 129) (uids-ascii-is-alpha (- i 129)))
            (write-word short q 0x400))
          (incr i)))
      (set *cbloc* p)))
    *cbloc*)

(defun stub ()
  "stubs that does nothing"
  (declare (external
            "setlocale"
            "bindtextdomain"
            "textdomain"
            "__cxa_atexit"
            "__ctype_get_mb_cur_max"
            "__do_global_dtors_aux")))
