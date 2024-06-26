(require string)
(require ascii)
(require atoi)
(require stdio)
(require simple-memory-allocator)
(require types)
(require upc-ascii)
(require upc-atoi)

(in-package posix)

(defparameter *cbloc-b* nil
  "the starting address of the cbloc-b arena")

(defparameter *cbloc-tolower* nil
  "the starting address of the cbloc-tolower arena")

(defparameter *cbloc-toupper* nil
  "the starting address of the cbloc-toupper arena")

(defun getenv (name)
  "finds a value of an environment variable with the given name"
  (declare (external "getenv" "secure_getenv"))
  (let ((p environ)
        (n (strlen name))
        (r (read-word ptr_t p))
        (result 0))
    (while (and (> (read-word ptr_t p) 0)
                (= result 0))
      (let ((s (read-word ptr_t p))
            (m (cast ptr_t (min n (strlen s)))))
        (if (not (memcmp s name m))
             (let ((x 1))
             ;; (upc-ocaml-debug 0xdeadc0de)
             (set result (ptr+1 char (strchr s (cast int ?=)))))
             nil)
        (set p (ptr+1 ptr_t p))))
    result))

(defun bzero (p n)
  "zero out a buffer of memory"
  (declare (external "bzero"))
  (let ((i 0))
    (while (< i n)
      (memory-write p 0)
      (incr p)
      (incr i))))

(defun rand ()
  (declare (external "rand"))
  (upc-ocaml-rand))

(defun upc-ascii-is-alpha (c)
  (or (and (>= c 0x41) (<= c 0x5a)) (and (>= c 0x61) (<= c 0x7a))))

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
              (upc-ocaml-debug j)
              (upc-ocaml-debug (ascii-is-alphanum j))
              (when (ascii-is-alpha j)
                (set mask (logor mask 0x400)))
              (when (ascii-is-digit j)
                (set mask (logor mask 0x800)))
              (when (ascii-is-alphanum j)
                (set mask (logor mask 0x8)))
              (when (ascii-is-whitespace j)
                (set mask (logor mask 0x2000)))
              (when (ascii-is-print j)
                (set mask (logor mask 0x4000)))
              (upc-ocaml-debug mask)
              (upc-ocaml-debug q)
              (write-word short q mask)))
          (incr i)))
      (set *cbloc-b* p)))
    *cbloc-b*)

(defun ctype-tolower-loc ()
  (declare (external "__ctype_tolower_loc"))
  (when (not *cbloc-tolower*)
    (let ((p (malloc (sizeof ptr_t)))
          (n (+ 129 255))
          (table (malloc (* (sizeof int32_t) n)))
          (start (ptr+ char table 129))
          (i 0))
      (bzero table (* (sizeof int32_t) n))
      (write-word ptr_t p start)
      (while (< i n)
        (let ((q (ptr+ int32_t table i))
              (mask 0))
          (when (>= i 129)
            (let ((j (cast char (- i 129))))
              (write-word int32_t q (ascii-to-lower j))))
          (incr i)))
      (set *cbloc-tolower* p)))
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

(defun upc-strtol (s endptr base)
  (declare (external "strtol"))
  (let ((v 0))
    (while (and (> (cast ptr_t (memory-read s)) 0)
                (not (= (cast ptr_t (memory-read s)) 0xa)))
      (set v (+ (* v base)  (upc-atoi-read-digit s)))
      (incr s))
    (when endptr
        (write-word ptr_t (cast ptr_t endptr) s))
    (cast int v)))

(defun rand ()
  (declare (external "rand"))
  (upc-ocaml-rand))

(defun round (x)
  (declare (external "round"))
  (upc-ocaml-round x))

;; Wide characters in uPolicyCraft are assumed to hold just ascii characters at the moment.

;; This should be configurable (i.e. have a OCaml function return the size from
;; the binary's OS).
(defparameter *wchar-size* 4
  "The size of a wide character on the current platform")

(defun wcslen (s)
  (declare (external "wcslen"))
  (let ((i 0)
        (eos false))
       (while (not eos)
         ;; Assume everything is in ascii.
         (let ((c (memory-read (+ s (* i *wchar-size*)))))
           (if (= (cast int c) 0)
             (set eos true)
             (incr i))))
  i))

(defun wcslen/with-null (s)
  (+ (wcslen s) 1))

(defun wcscmp (wcs1 wcs2)
  (declare (external "wcscmp"))
  (memcmp wcs1 wcs2 (min (wcslen/with-null wcs1)
                         (wcslen/with-null wcs2))))

(defun wcscpy (dst src)
  (declare (external "wcscpy"))
  (memcpy dst src (* (wcslen/with-null src) *wchar-size*)))

(defun wcscat (dst src)
  (declare (external "wcscat"))
  (let ((n (wcslen dst))
        (dst1 (+ dst (* *wchar-size* n)))
        (s (wcscpy dst1 src)))
     dst))

(defun wcschr (ws wc)
  (declare (external "wcschr"))
  (memchr ws (cast char wc) (* (wcslen ws) *wchar-size*)))

(defun write-char-into-wide-char (dest i c)
  (memory-write (+ dest (* i *wchar-size*)) c)
  (let ((j 1))
    (while (< j *wchar-size*)
      (memory-write (+ dest (+ (* i *wchar-size*) j)) 0)
      (incr j))))

(defun mbstowcs (dest src n)
  (declare (external "mbstowcs"))
  (if dest
    (let ((i 0)
          (eos false))
      (while (and (< i n) (not eos))
        (let ((c (memory-read (+ src i))))
          (when (= (cast int c) 0)
            (set eos true))
          (write-char-into-wide-char dest i c)
          (when (not (= (cast int c) 0))
            (incr i))))
      i)
    (let ((i 0)
          (eos false))
      (while (not eos)
        (let ((c (memory-read (+ src i))))
          (if (= (cast int c) 0)
            (set eos true)
            (incr i))))
      i)))

(defun setlocale (category locale)
  (declare (external "setlocale"))
  (let ((locale (malloc 16)))
    (memory-write locale ?C)
    (memory-write (ptr+ char locale 1) 0)
    locale))
