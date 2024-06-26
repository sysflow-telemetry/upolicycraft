
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
  (let ((fd (upc-channel-open fname 0)))
    (write-word ptr_t newf fd))
  0)

(defun apr-file-info-get (finfo wanted thefile)
  (declare (external "apr_file_info_get"))
  ;; Always make the file appear regular.
  (write-word int (+ finfo 0x10) 1)
  0)

;;(defun apr-file-gets (ptr len str)
;;  (declare (external "apr_file_gets"))
;;  (let ((i 0)
;;        (eof false)
;;        (proceed true))
;;    (while (and (< i len) (not eof) proceed)
;;      (let ((r (fgets-step ptr len str i)))
;;        (if (= r 0) (set eof true)
;;          (set proceed r)))
;;      (incr i))
;;    (memory-write (+ ptr (min (-1 len) (+ ptr i))) 0:8)
;;    (if eof (+ 20000 50000 14)
;;     0)))
