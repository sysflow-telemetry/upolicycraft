(require string)
(require atoi)
(require stdio)
(require simple-memory-allocator)
(require types)

(defparameter *cbloc* nil
  "the starting address of the cbloc-arena")

(defun getenv (name)
  "finds a value of an environment variable with the given name"
  (declare (external "getenv"))
  (let ((p environ))
    (while (and (not (points-to-null p))
                (/= (strcmp p name) 0))
      (ptr+1 ptr_t p))
    (if p (strchr p (cast int ?=)) p)))

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

(defun ctype-b-loc ()
  (declare (external "__ctype_b_loc"))
  (if (not *cbloc*)
		(let ((n (+ 129 255))
					(p (malloc (* (sizeof ptr_t) n)))
					(i 0))
			(while (< i n)
			 (let ((q (ptr+ ptr_t p i))
						 (chunk (malloc 8)))
				(write-word ptr_t q chunk))
			 (incr i))
      (set *cbloc* (ptr+ ptr_t p 129)))
  *cbloc*))

(defun stub ()
  "stubs that does nothing"
  (declare (external
            "setlocale"
            "bindtextdomain"
            "textdomain"
            "__cxa_atexit"
            "__ctype_get_mb_cur_max"
            "__do_global_dtors_aux")))
