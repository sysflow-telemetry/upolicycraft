(require types)
(require pointers)
(require memory)
(require ascii)

(in-package posix)

(defun strlen (p)
  (declare (external "strlen"))
  (let ((len 0))
    (while (not (points-to-null p))
      (incr len p))
    len))


(defun strcpy (dst src)
  (declare (external "strcpy"))
  (let ((dst dst))
    (while (not (points-to-null src))
      (copy-byte-shift dst src))
    (memory-write dst 0:8))
  dst)

(defun strncpy (dst src len)
  (declare (external "strncpy"))
  (let ((dst dst))
    (while (and len (not (points-to-null src)))
      (decr len)
      (copy-byte-shift dst src))
    (memory-write dst 0:8))
  dst)

(defun strdup (src)
  (declare (external "strdup"))
  (let ((dst (malloc (+1 (strlen src)))))
    (and dst (strcpy dst src))))


(defun memmove (dst src len)
  (declare (external "memmove"))
  (let ((a dst) (b src))
    (when (/= src dst)
      (if (> src dst) (copy-right a b len)
        (+= a (-1 len))
        (+= b (-1 len))
        (copy-left a b len))))
  dst)

(defun memcpy (dst src len)
  (declare (external "memcpy"))
  (copy-right dst src len))

(defun memccpy (dst src c len)
  (declare (external "memccpy"))
  (let ((found 0))
    (while (and len (not found))
      (copy-byte dst src)
      (incr src)
      (decr len)
      (when (points-to char dst c)
        (set found (+ 1 dst)))
      (incr dst))
    found))

(defmacro find-character (dir p c n)
  (declare (visibility :private))
  (prog
      (while (and n (not (points-to char p c)))
        (decr n)
        (dir p))
     (if (points-to char p c) p 0)))

(defun memchr (p c n)
  (declare (external "memchr"))
  (find-character incr p c n))


(defun memrchr (p c n)
  (declare (external "memrchr"))
  (find-character decr p c n))


(defun strchr (p c)
  (declare (external "strchr" "index"))
  (memchr p c (+ (strlen p) 1)))

(defun strrchr (p c)
  (declare (external "strrchr" "rindex"))
  (memrchr p c (+ (strlen p) 1)))


(defun strpbrk (str set)
  (declare (external "strpbrk"))
  (let ((p set) (found 0))
    (while (and
            (not (points-to-null p))
            (not found))
      (set found (strchr str (cast int (memory-read p))))
      (incr p))
    found))

(defun strcat (dst src)
  (declare (external "strcat"))
  (strcpy (+ dst (cast ptr_t (strlen dst))) src)
  dst)

(defun strncat (dst src len)
  (declare (external "strncat"))
  (strncpy (+ dst (cast ptr_t (strlen dst))) src len)
  dst)


(defun memset (p c n)
  (declare (external "memset"))
  (let ((p p))
    (while n
      (memory-write p c)
      (incr p)
      (decr n)))
  p)

(defun memcmp (p1 p2 n)
  (declare (external "memcmp"))
  (let ((res 0) (i 0))
    (while (and (< i n) (not res))
      (set res (compare (cast int (memory-read p1)) (cast int (memory-read p2))))
      (incr p1 p2 i))
    res))

(defun strlen/with-null (s)
  "returns a length of the string S
   (including the terminating null character)"
  (declare (visibility :private))
  (+1 (strlen s)))

(defun strncmp (s1 s2 n)
  (declare (external "strncmp"))
  (memcmp s1 s2 (min n
                     (strlen/with-null s1)
                     (strlen/with-null s2))))

(defun strcmp (s1 s2)
  (declare (external "strcmp" "strcoll"))
  (memcmp s1 s2 (min (strlen/with-null s1)
                     (strlen/with-null s2))))

(defun memcasecmp (p1 p2 n)
  (let ((res 0) (i 0))
    (while (and (< i n) (not res))
      (set res (compare
                (ascii-to-lower (cast int (memory-read p1)))
                (ascii-to-lower (cast int (memory-read p2)))))
      (incr p1 p2 i))
    res))

(defun strncasecmp (p1 p2 n)
  (declare (external "strncasecmp"))
  (memcasecmp p1 p2 (min n
                         (strlen/with-null p1)
                         (strlen/with-null p2))))

(defun strcasecmp (p1 p2)
  (declare (external "strcasecmp"))
  (strncasecmp p1 p2 (min (strlen/with-null p1)
                          (strlen/with-null p2))))

(defmacro find-substring (compare hay needle)
  (declare (visibility :private))
  (let ((found 0)
        (n (strlen needle)))
    (while (and (memory-read hay) (not found))
      (upc-ocaml-debug hay)
      (if (not (compare hay needle n))
        (set found 1)
        (incr hay)))
    (if found hay 0)))

(defun strstr (hay needle)
  (declare (external "strstr"))
  (find-substring strncmp hay needle))

(defun strcasestr (hay needle)
  (declare (external "strcasestr"))
  (find-substring strncasecmp hay needle))

(defmacro strspn (str set)
  (declare (external "strspn"))
  (let ((len 0))
    (while (strpbrk str set)
      (incr str len))
    len))

(defun strcspn (str set)
  (declare (external "strcspn"))
  (let ((len 0))
    (until (or (points-to-null str)
               (strpbrk str set))
      (incr str len))
    len))


(defun strsep (strp sep)
  (declare (external "strsep"))
  (let ((str (read-word ptr_t strp))
        (pos (and str (+ str (strcspn str sep)))))
    (when str
      (if (points-to-null pos)
          (write-word ptr_t strp 0)
        (memory-write pos 0)
        (write-word ptr_t strp (+1 pos))))
    str))


(defun strxfrm (dst src len)
  (declare (external "strxfrm"))
  (strncpy dst src len)
  len)

(defun stpcpy (dst src)
  (declare (external "stpcpy"))
  (let ((len (strlen src)))
    (+ (memcpy dst src (+1 len)) (cast ptr_t len))))
