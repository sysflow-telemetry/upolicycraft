(require string)
(require ascii)
(require atoi)
(require stdio)
(require simple-memory-allocator)
(require types)

(defparameter *cbloc-b* nil
  "the starting address of the cbloc-b arena")

(defparameter *cbloc-tolower* nil
  "the starting address of the cbloc-tolower arena")

(defparameter *cbloc-toupper* nil
  "the starting address of the cbloc-toupper arena")

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
        ;; (uids-ocaml-debug 0xfabc0de)
        ;; (uids-ocaml-debug n)
        ;; (uids-ocaml-debug (strlen s))
        (if (not (memcmp s name m))
             (let ((x 1))
             ;; (uids-ocaml-debug 0xdeadc0de)
             (set result (ptr+1 char (strchr s (cast int ?=)))))
             nil)
        (set p (ptr+1 ptr_t p))))
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

;; space $0x2000
;; print $0x4000

(defun ctype-b-loc ()
  (declare (external "__ctype_b_loc"))
  (when (not *cbloc-b*)
    (let ((p (malloc (sizeof ptr_t)))
          (n (+ 129 255))
          (table (malloc (* (sizeof short) n)))
          (start (ptr+ short table 129))
          (i 0))
      (bzero table (* (sizeof short) n))
      (write-word ptr_t p start)
      (while (< i n)
        (let ((q (ptr+ short table i))
              (mask 0))
          (when (>= i 129)
            (let ((j (cast char (- i 129))))
              ;; (uids-ocaml-debug j)
              (when (ascii-is-alpha j)
                (set mask (logor mask 0x400)))
              (when (ascii-is-digit j)
                (set mask (logor mask 0x800)))
              (when (ascii-is-alphanum j)
                (set mask (logor mask 0x8)))
              (when (ascii-whitespace j)
                (set mask (logor mask 0x2000)))
              (when (ascii-is-print j)
                (set mask (logor mask 0x4000)))
              (write-word short q mask)))
          (incr i)))
      (set *cbloc-b* p)))
    *cbloc-b*)

;; (uids-ocaml-debug j)

(defun ctype-tolower-loc ()
  (declare (external "__ctype_tolower_loc"))
  (when (not *cbloc-tolower*)
    ;; (uids-ocaml-debug 0xfabc0de)
    ;; (uids-ocaml-debug (sizeof int32_t))
    (let ((p (malloc (sizeof ptr_t)))
          (n (+ 129 255))
          (table (malloc (* (sizeof int32_t) n)))
          (start (ptr+ char table 129))
          (i 0))
      (bzero table (* (sizeof int32_t) n))
      (write-word ptr_t p start)
      (while (< i n)
	;; (uids-ocaml-debug i)
        (let ((q (ptr+ int32_t table i))
              (mask 0))
          (when (>= i 129)
            (let ((j (cast char (- i 129))))
	      (write-word int32_t q (ascii-to-lower j))))
          (incr i)))
      (set *cbloc-tolower* p)))
    ;; (uids-ocaml-debug 0xfabc1de)
    ;; (uids-ocaml-debug *cbloc-tolower*)
    *cbloc-tolower*)

(defun ctype-toupper-loc ()
  (declare (external "__ctype_toupper_loc"))
  (when (not *cbloc-toupper*)
    (let ((p (malloc (sizeof ptr_t)))
          (n (+ 129 255))
          (table (malloc (* (sizeof char) n)))
          (start (ptr+ char table 129))
          (i 0:64))
      (bzero table (* (sizeof char) n))
      (write-word ptr_t p start)
      (while (< i n)
        (let ((q (ptr+ char table i))
              (mask 0))
          (when (>= i 129)
            (let ((j (cast char (- i 129))))
	      (memory-write q (ascii-to-upper j))))
          (incr i)))
      (set *cbloc-toupper* p)))
    *cbloc-toupper*)

(defun stub ()
  "stubs that does nothing"
  (declare (external
            "setlocale"
            "bindtextdomain"
            "textdomain"
            "__cxa_atexit"
            "__ctype_get_mb_cur_max"
            "__do_global_dtors_aux")))
