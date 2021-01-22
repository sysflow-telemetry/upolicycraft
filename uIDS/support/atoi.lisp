(require types)
(require ascii)
(require pointers)
(require simple-memory-allocator)
(require stdio)
(require stdlib)
(require string)

(defparameter *access-used* 2
  "has access been called")

(defmacro skip-all (pred s)
  (while (pred (memory-read s)) (incr s)))

(defun atoi-prefix (s)
  (or (ascii-special s) (ascii-whitespace s)))

(defun atoi-read-digit (s)
  (cast ptr_t (- (memory-read s) ?0)))

(defun read-ascii-word (s)
  (skip-all atoi-prefix s)
  (let ((v 0)
        (sign (ascii-sign (memory-read s))))
    (while (ascii-digit (memory-read s))
      (set v (+ (* v 10) (atoi-read-digit s)))
      (incr s))
    (* sign v)))

(defmacro make-converter (type s)
  (cast type (read-ascii-word s)))

(defun atoi (s)
  (make-converter int s))

(defun my-atoi-read-digit (s)
  (cast int (- (memory-read s) ?0)))

(defun uids-allocate (n isx addr)
  (declare (external "allocate"))
  (let ((buf (malloc n)))
     (write-word ptr_t (cast ptr_t addr) buf)
   0))

(defun succ (n)
  (+ n 1))

(defun uids-atoi (s)
  (declare (external "atoi"))
  (let ((v 0))
    (while (and (> (cast ptr_t (memory-read s)) 0)
                (not (= (cast ptr_t (memory-read s)) 0xa)))
      (set v (+ (* v 10)  (atoi-read-digit s)))
      (incr s))
    (cast int v)))

(defun uids-itoa (n)
  (declare (external "itoa"))
  (let ((chunk (malloc 16)))
   (memory-write chunk 0x30)
   (memory-write (succ chunk) 0x0)
   chunk))

(defun htons (v)
  (declare (external "htons"))
  v)

(defun uids-mmap (addr len prot flags fd offset)
  (declare (external "mmap"))
  (malloc len))

;; Always return success
(defun apr-app-initialize (argc argv env)
  (declare (external "apr_app_initialize"))
  0)

(defun apr-pool-create-ex (newpool parent abortfn allocator)
  (declare (external "apr_pool_create_ex"))
  0)

(defun apr-palloc (p sz)
  (declare (external "apr_palloc"))
  (malloc sz))

(defun apr-generate-random-bytes (buf l)
  (declare (external "apr_generate_random_bytes"))
  0)

(defun apr-random-insecure-ready (r)
  (declare (external "apr_random_insecure_ready"))
  0)

(defun apr-pstrdup (p s)
  (declare (external "apr_pstrdup"))
  (strdup s))

(defun assert-fail (assertion file line func)
   (declare (external "__assert_fail"))
   (fputs assertion *standard-error*)
   (fputs file *standard-error*)
   (fputs line *standard-error*)
   (fputs func *standard-error*))

(defun apr-hash-make (p)
  (declare (external "apr_hash_make"))
  (malloc 1))

(defun apr-hash-get (ht key klen)
  (declare (external "apr_hash_get"))
  (dict-get ht key))

(defun apr-hash-set (ht key klen val)
  (declare (external "apr_hash_set"))
  (dict-add ht key val))

(defun apr-array-make (p n size)
  (declare (external "apr_array_make"))
  (let ((p (malloc 1)))
    (array-make p size)
    p))

(defun apr-array-push (arr)
  (declare (external "apr_array_push"))
  (let ((sz (array-elt-size arr))
        (p (malloc sz)))
    (array-push arr p)
    p))

(defun apr-array-pop (arr)
  (declare (external "apr_array_pop"))
  (array-pop arr))

(defun apr-getopt-init (os cont argc argv)
  (declare (external "apr_getopt_init"))
  (let ((p (malloc 256)))
    (memset p 0 256)
    (write-word ptr_t os p)
  0))

(defun apr-getopt (os opts option-ch option-arg)
  (declare (external "apr_getopt"))
  (+ 20000 50000 14))

(defun apr-sockaddr-info-get (sa hostname family port flags p)
  (declare (external "apr_sockaddr_info_get"))
  0)

(defun apr-file-open (newf fname flag perm pool)
  (declare (external "apr_file_open"))
  (let ((fd (channel-open fname)))
    (write-word ptr_t newf fd))
  0)

(defun apr-file-info-get (finfo wanted thefile)
  (declare (external "apr_file_info_get"))
  ;; Always make the file appear regular.
  (write-word int (+ finfo 0x10) 1)
  0)

(defun apr-file-gets (ptr len str)
  (declare (external "apr_file_gets"))
  (let ((i 0)
        (eof false)
        (proceed true))
    (while (and (< i len) (not eof) proceed)
      (let ((r (fgets-step ptr len str i)))
        (if (= r 0) (set eof true)
          (set proceed r)))
      (incr i))
    (memory-write (+ ptr (min (-1 len) (+ ptr i))) 0:8)
    (if eof (+ 20000 50000 14)
     0)))

(defun uids-access (path mode)
  (declare (external "access"))
    (if (= *access-used* 0)
      0
      (let ()
        (decr *access-used*)
        -1)))

;;  (if (= *access-used* nil)
;;      (let ()
;;        (set *access-used* 1)
;;        -1)
;;      (let ()
;;        (set *access-used* nil)
;;        0))

(defun uids-scanf (fmt a)
  (declare (external "__isoc99_scanf"))
   (let ((x (uids-ocaml-scanf fmt)))
      (write-word ptr_t a x)
   0))
;
(defun uids-snprintf (s sz fmt addr)
   (declare (external "snprintf"))
   (let ((m (malloc sz)))
     (uids-ocaml-snprintf s sz fmt addr)
   0))

(defun uids-sprintf (s fmt addr)
   (declare (external "sprintf"))
   (uids-ocaml-sprintf s fmt addr)
   (strlen s))

;;(defun strcspn (p n)
;;  (declare (external "strcspn"))
;;  (let ((i 0)
;;        (s p))
;;     (while (and (> (cast ptr_t (memory-read s)) 0)
;;                (not (= (cast ptr_t (memory-read s)) 0xa)))
;;      (incr i)
;;      (incr s))
;;      i))

(defun ferror (fp)
    (declare (external "ferror"))
    0)

;;(let ((p (malloc (* (sizeof ptr_t) 384)))
;;        (q p)
;;        (i 0))
;;    (while (< i 384)
;;      (write-word ptr_t q (malloc 8))
;;      (set q (ptr+1 ptr_t q))
;;      (incr i))
;;   (ptr+ ptr_t p 128))

;;(defun ctype-b-loc ()
;;  (declare (external "__ctype_b_loc"))
;;  nil)

(defun atol  (s) (make-converter long s))
(defun atoll (s) (make-converter long-long s))

(defun term (code)
  (declare (external "terminate"))
  (exit-with code))

(defun random-cgc (buf n rnd-bytes)
  (declare (external "random_cgc"))
  0)
