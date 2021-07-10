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
  (let ((digit (memory-read s)))
  (if (< digit ?a)
     (if (< digit ?A)
       (cast ptr_t (- digit ?0))
       (cast ptr_t (+ (- digit ?A) 10:8)))
     (cast ptr_t (+ (- digit (cast char ?a)) 10:8)))))

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

(defun getpagesize ()
  (declare (external "getpagesize"))
  4096)

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

(defun uids-strtol (s endptr base)
  (declare (external "strtol"))
  (let ((v 0))
    (while (and (> (cast ptr_t (memory-read s)) 0)
                (not (= (cast ptr_t (memory-read s)) 0xa)))
      (set v (+ (* v base)  (atoi-read-digit s)))
      (incr s))
    (when endptr
        (write-word ptr_t (cast ptr_t endptr) s))
    (cast int v)))

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
  (let ((fd (uids-channel-open fname 0)))
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
   (uids-ocaml-sprintf s fmt)
   (strlen s))

(defun epoll-wait (epfd events nevents timeout)
   (declare (external "epoll_wait"))
   nevents)

(defmacro open-socket-fd ()
    (let ((fname (malloc 16)))
      (uids-ocaml-debug 0xdeadc00de)
      ;;(write-word ptr_t fname 0x3e74656e3c) ;; <net>
      (write-word char fname 0x73)
      (write-word char (+ fname 1) 0x72)
      (write-word char (+ fname 2) 0x76)
      (write-word char (+ fname 3) 0x0)
      ;; (puts fname)
      (let ((fd (uids-channel-open-network fname)))
        (uids-ocaml-add-socket fd)
        fd)))

(defun socketpair (domain type protocol socket-vector)
  (declare (external "socketpair"))
  (let ((fd (open-socket-fd)))
    (write-word int32_t socket-vector fd)
    (write-word int32_t (+ socket-vector (sizeof int32_t)) fd)
    0))

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

(defun uids-socket (domain tp protocol)
  (declare (external "socket"))
  (open-socket-fd))

(defun reverse-string (start end)
  (let ((p start)
        (q end))
     (while (< p q)
       (let ((r (memory-read p)))
         (memory-write p (memory-read q))
         (memory-write q r)
         (incr p)
         (decr q)))))

(defun uids-itoa (buf n)
  (declare (external "itoa1"))
  (let ((started false)
        (begin buf))
    (while (or (> n 0) (not started))
      (set started true)
      (let ((digit (mod n 10)))
        (uids-ocaml-debug digit)
        (write-word char buf (+ (cast char digit) ?0))
        (set n (/ n 10))
        (incr buf)))
  (write-word char buf 0x0)
  (reverse-string begin (- buf 1))))

(defparameter *accept-used* 0
  "has accept been called")

(defparameter *network-test-case* 0
  "The current test case")

(defun uids-accept (sock addr addrlen)
  (declare (external "accept"))
  (let ((af-inet 2)
        (no-test-cases (uids-ocaml-network-test-cases)))
    ;; Write af-inet into the sock-addr
    (write-word int addr af-inet)
    (write-word int (+ addr 4) 0x100007f)
    (write-word ptr_t addrlen 16)
    (if (= *network-test-case* no-test-cases)
      -1
      (let ((fname (malloc 16)))
      ;;(write-word ptr_t fname 0x3e74656e3c) ;; <net>
        (write-word char fname 0x6e)
        (write-word char (+ fname 1) 0x65)
        (write-word char (+ fname 2) 0x74)
        (uids-itoa (+ fname 3) *network-test-case*)
        ;;(write-word char (+ fname 3) 0x0)
        ;; (puts fname)
        (incr *network-test-case*)
        (let ((fd (uids-channel-open-network fname)))
          (uids-ocaml-network-fd sock fd)
          fd)))))

      ;;(let ()
      ;;  (decr *accept-used*)
      ;;  fd))))


;; char    *pw_name   User's login name.
;; uid_t    pw_uid    Numerical user ID.
;; gid_t    pw_gid    Numerical group ID.
;; char    *pw_dir    Initial working directory.
;; char    *pw_shell  Program to use as shell.

(defun uids-getuser-struct (name)
  (let ((len (* (sizeof ptr_t) 5))
        (user-offset (sizeof ptr_t))
        (group-offset (* (sizeof ptr_t) 2))
        (dir-offset 32)
        (m (malloc len))
        (homedir (malloc 10)))
    (write-word char homedir 0x2F)
    (write-word char (+ homedir 1) 0x68)
    (write-word char (+ homedir 2) 0x00)
    (write-word ptr_t (cast ptr_t (+ m user-offset)) 33)
    (write-word ptr_t (cast ptr_t (+ m group-offset)) 33)
    (write-word ptr_t (cast ptr_t (+ m dir-offset)) homedir)
    m))

(defun uids-getgrnam (name)
  (declare (external "getgrnam"))
  (uids-getuser-struct name))

(defun uids-getpwnam (login)
  (declare (external "getpwnam"))
  (uids-getuser-struct login))

;;  struct stat {
;;  dev_t     st_dev;         /* ID of device containing file */
;;  ino_t     st_ino;         /* Inode number */
;;  mode_t    st_mode;        /* File type and mode */
;;  nlink_t   st_nlink;       /* Number of hard links */
;;  uid_t     st_uid;         /* User ID of owner */
;;  gid_t     st_gid;         /* Group ID of owner */
;;  dev_t     st_rdev;        /* Device ID (if special file) */
;;  off_t     st_size;        /* Total size, in bytes */
;;  blksize_t st_blksize;     /* Block size for filesystem I/O */
;;  blkcnt_t  st_blocks;      /* Number of 512B blocks allocated */

(defun epoll-create (x)
  (declare (external "epoll_create"))
  (uids-ocaml-debug 0xccccd)
  1024)

(defun uids-fstat (fd buf)
  (declare (external "fstat64" "__fstat"))
  (let ((offset 48))
    ;; (write-word ptr_t (+ buf offset) size)
    (uids-ocaml-fstat fd buf)))

(defun uids-stat (filename buf)
  (declare (external "stat64" "__stat"))
  (uids-ocaml-debug 0xf0bc0de)
  (let ((offset 48))
    ;; (write-word ptr_t (+ buf offset) size)
    (uids-ocaml-stat filename buf)))

;; (defun uids-stat (filename buf)
;;   (declare (external "__stat"))
;;   (uids-debug-ocaml 0xf00bc0de)
;;   (let ((offset 48))
;;     ;; (write-word ptr_t (+ buf offset) size)
;;     (uids-ocaml-stat filename buf)))

(defun uids-xstat (vers path buf)
  (declare (external "__xstat"))
  (let ((fd (uids-channel-open path 0)))
    (if (= fd -1)
      -1
      (let ()
        (uids-channel-close fd)
        0))))

(defparameter *uids-errno* 0
  "uIDS errno")

(defun uids-errno ()
  (declare (external "__errno_location"))
  (when (not *uids-errno*)
     (set *uids-errno* (malloc (sizeof int)))
     (write-word ptr_t *uids-errno* 0))
  (uids-ocaml-debug 0xeee)
  (uids-ocaml-debug *uids-errno*)
  *uids-errno*)

(defun uids-mprotect (addr len prot)
  (declare (external "mprotect"))
  0)

(defun uids-munmap (addr len)
  (declare (external "munmap"))
  0)

;; struct tm {
;;     int tm_sec;    /* Seconds (0-60) */
;;     int tm_min;    /* Minutes (0-59) */
;;     int tm_hour;   /* Hours (0-23) */
;;     int tm_mday;   /* Day of the month (1-31) */
;;     int tm_mon;    /* Month (0-11) */
;;     int tm_year;   /* Year - 1900 */
;;     int tm_wday;   /* Day of the week (0-6, Sunday = 0) */
;;     int tm_yday;   /* Day in the year (0-365, 1 Jan = 0) */
;;     int tm_isdst;  /* Daylight saving time */
;; }

(defun uids-localtime-r (time tm)
  (declare (external "localtime_r"))
  (memset tm 0 (* (sizeof int) 9)))

(defun uids-localtime (time)
  (declare (external "localtime"))
  (let ((time-struct-size (* (sizeof int) 9))
       (m (malloc time-struct-size)))
  (memset m 0 time-struct-size)))

(defun uids-gmtime (tm)
  (declare (external "gmtime"))
  tm)

(defun uids-fork ()
  (declare (external "fork"))
  0)

(defun uids-getuid ()
  (declare (external "getuid"))
  (uids-ocaml-getuid))

(defun uids-setsockopt (fd level opt optval len)
  (declare (external "setsockopt"))
  0)

(defun uids-sigfillset (st)
  (declare (external "sigfillset"))
  0)

(defun uids-sigaction (sm act oldact)
  (declare (external "sigaction"))
  0)

(defun uids-dup2 (oldfd newfd)
  (declare (external "dup2"))
  (uids-ocaml-dup2 oldfd newfd)
  newfd)

(defun uids-getpeername (sockfd sockaddr socklen)
  (declare (external "getpeername"))
  (write-word short sockaddr 2)
  (write-word int (+ sockaddr 4) 0x100007f)
  (write-word ptr_t socklen 16)
  0)

(defun uids-getsockname (sockfd sockaddr socklen)
  (declare (external "getsockname"))
  (write-word short sockaddr 2)
  (write-word ptr_t socklen 16)
  0)

(defun uids-setrlimit (resource rlim)
  (declare (external "setrlimit64"))
  0)

(defun uids-initgroups (name gid)
  (declare (external "initgroups"))
  0)

(defun uids-chdir (name)
  (declare (external "chdir"))
  0)

(defun uids-chroot (name)
  (declare (external "chroot"))
  0)

(defun uids-gettimeofday (tv tz)
  (declare (external "gettimeofday"))
  0)

(defun uids-inet-aton (str inp)
  (declare (external "inet_aton"))
  (let ((localhost 0x100007f))
    (write-word ptr_t inp localhost)
    localhost))

(defun uids-bind (fd addr len)
  (declare (external "bind"))
  0)

(defun uids-fchmod (fd mode)
  (declare (external "fchmod"))
  0)

(defun uids-fchown (fd owner group)
  (declare (external "fchown"))
  0)

(defun uids-mkdir (path mode)
  (declare (external "mkdir"))
  0)

;; getopt

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

(defun uids-nl-langinfo (item)
  (declare (external "nl_langinfo"))
  (let ((buf (malloc 64)))
    (memory-write buf 0x41)
    (memory-write (ptr+1 char buf) 0x00)
    buf))

(defun uids-opendir (path)
  (declare (external "opendir"))
  (uids-ocaml-opendir path))

(defparameter dirent (malloc 512))

(defun uids-readdir (dir)
  (declare (external "readdir"))
  (uids-ocaml-readdir dir dirent))

(defun uids-regcomp (preg regex cflags)
  (declare (external "regcomp"))
  0)

(defun uids-regexec (preg str nm pm eflags)
  (declare (external "regexec"))
  0)
