(require types)
(require ascii)
(require simple-memory-allocator)
(require string)

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

(defun uids-atoi (s)
  (declare (external "atoi"))
  (let ((v 0))
    (while (> (cast ptr_t (memory-read s)) 0)
      (set v (+ (* v 10)  (atoi-read-digit s)))
      (incr s))
    (cast int v)))

(defun htons (v)
  (declare (external "htons"))
  v)

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

(defun atol  (s) (make-converter long s))
(defun atoll (s) (make-converter long-long s))
