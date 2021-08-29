(require libc-init)
(require memory)
(require pointers)
(require types)

(defun fputc (char stream)
  (declare (external "fputc" "putc"))
  (if (= 0 (uids-channel-output stream char)) char -1))

(defun putchar (char)
  (declare (external "putchar"))
  (fputc char *uids-standard-output*))

(defun fputs (p stream)
  (declare (external "fputs"))
  (while (not (points-to-null p))
    (fputc (cast int (memory-read p)) stream)
    (incr p))
  (fputc 0xA stream))

(defun puts (p)
  (declare (external "puts"))
  (fputs p *uids-standard-output*)
  (uids-channel-flush *uids-standard-output*))

(defun put (p)
  (declare (external "put"))
  (fputs p *uids-standard-output*)
  (uids-channel-flush *uids-standard-output*))


;; the channel module have rough equality between streams and
;; file descriptors, as they both are represented as integers. We are currently
;; ignoring modes, we will add them later, of course.
(defun fopen (path mode)
  (declare (external "fopen" "open" "open64" "creat" "fopen64"))
  (uids-channel-open path mode))

(defun uids-fdopen (fd mode)
   (declare (external "fdopen"))
   fd)

;; Primus treats file descriptors and file handles interchangeably.
(defun fileno (fd)
  (declare (external "fileno"))
  fd)

;;(defun open3 (path flags mode)
;;  (declare (external "open"))
;;  (fopen path mode))

(defun output-item-nth-char (ptr size item fd i)
  (= 0 (uids-channel-output
        fd
        (memory-read (+ ptr (* size item) i)))))

(defun output-item (buf size item fd)
  (let ((i 0))
    (while (and
            (< i size)
            (output-item-nth-char buf size item fd i))
      (incr i))
    i))

(defun fwrite (buf size n stream)
  (declare (external "fwrite"))
  (let ((i 0)
       (stream1 (uids-ocaml-check-dup2 stream)))
    (while (and (< i n)
                (= size (output-item buf size i stream1)))
      (incr i))
    i))

(defun write (fd buf cnt)
  (declare (external "write"))
  (let ((written (fwrite buf 1 cnt fd))
        (failure (uids-channel-flush fd)))
    (or failure written)))

(defun write-stdout (buf cnt)
   (declare (external "write0"))
   (write (cast int32_t *uids-standard-output*) buf cnt))

;; When cache is true, always pull from the descriptor
;;(defun uids-channel-input (desc cache)
;;  (if cache
;;    (uids-channel-input desc)
;;    (let ((cc (uids-ocaml-cache-input)))
;;      (if (= cc -1)
;;        (uids-channel-input desc)
;;        cc))))

(defun input-item-nth-char (ptr size item desc i)
  (let ((c (uids-channel-input desc)))
    (if (= c -1) 0
      (memory-write (+ ptr (* size item) i) (cast char c))
      1)))

(defun input-item (buf size item fd)
  (let ((i 0))
    (while (and (< i size)
                (input-item-nth-char buf size item fd i))
      (incr i))
    i))

;; Assume flags has MSG_PEEK set.
(defun recv (stream ptr n flags)
  (declare (external "recv"))
  (let ((i 0)
        (stream1 (uids-ocaml-check-dup2 stream))
        (pos (uids-channel-offset stream1)))
    ;; (uids-ocaml-debug 0xfeabc0de)
    ;; (uids-ocaml-debug pos)
    (while (and
            (< i n)
            (= 1 (input-item ptr 1 i stream1)))
      (incr i))
    (uids-channel-seek stream1 pos)
    ;; (uids-ocaml-debug (uids-channel-offset stream1))
    i))

(defun fread (ptr size n stream)
  (declare (external "fread"))
  (let ((i 0)
        (stream1 (uids-ocaml-check-dup2 stream)))
    (while (and
            (< i n)
            (= size (input-item ptr size i stream1)))
      (incr i))
    i))

(defun pread64 (fd buf size offset)
  (declare (external "pread64"))
  ;;(let ((skip 0)
  ;;      (n 0))
  ;;  (while (< skip offset)
  ;;    (channel-input fd)
  ;;    (incr skip))
  ;;  (uids-ocaml-debug 0xababa))
  (fread buf 1 size fd))

(defun pwrite64 (fd buf size offset)
  (declare (external "pwrite64"))
  (fwrite buf 1 size fd))

(defun read (fd buf n)
  (declare (external "read"))
  (fread buf 1 n fd))

(defun fgetc (stream)
  (declare (external "fgetc" "getc" "_IO_getc"))
  (uids-channel-input stream))

(defun ungetc (char stream)
  (declare (external "ungetc"))
  (let ((pos (uids-channel-offset stream)))
    (uids-channel-seek stream (- pos 1))))

(defun fclose (stream)
  (declare (external "fclose" "close"))
  0)

(defun ftell (stream)
  (declare (external "ftell"))
  (uids-channel-offset stream))

(defun fseek (stream offset whence)
  (declare (external "fseek"))
  (uids-channel-seek stream offset))

(defun fgets-step (ptr len str i)
  (let ((c (uids-channel-input str)))
    (if (= c -1) 0
      (memory-write (+ ptr i) (cast char c))
      (not (= c 0xA:8)))))

(defun fgets (ptr len str)
  (declare (external "fgets"))
  (let ((i 0))
    (while (and (< i len)
                (fgets-step ptr len str i))
      (incr i))
    (memory-write (+ ptr (min (-1 len) i)) 0:8)
    ptr))

(defun getchar ()
  (declare (external "getchar"))
  (fgetc *uids-standard-input*))

(defun uids-printf (fmt)
  (declare (external "printf"))
  (puts fmt)
  (uids-channel-flush *uids-standard-output*)
  0)

(defmethod machine-kill ()
  (uids-channel-flush *uids-standard-output*)
  (uids-channel-flush *uids-standard-error*))

(defun transmit (fd buf count tx-bytes)
  (declare (external "transmit"))
  (fwrite buf count 1 fd)
  (write-word int tx-bytes count)
  0)

;; (n (fread buf count 1 fd))

(defun receive-step (ptr len str i)
  (let ((c (uids-channel-input str)))
    (if (= c -1) 0
      (memory-write (+ ptr i) (cast char c)))))

(defun receive (fd buf count rx-bytes)
  (declare (external "receive"))
  (let ((z 0))
    (while (and (< z count)
                (receive-step buf count fd z))
      (incr z))
      (write-word ptr_t (cast ptr_t rx-bytes) z)
    0))

;; It would have been nice to implement this on top of receive,
;; but for some reason Primus's typechecker complains.
(defun receive-bytes (buf count)
   (declare (external "receive_bytes"))
   (let ((z 0))
     (while (and (< z count)
                 (receive-step buf count *uids-standard-input* z))
      (incr z))
    0))

(defun transmit-all (fd buf size)
  (declare (external "transmit_all"))
  (write fd buf size)
  0)

(defun receive-delim (fd buf size delim)
    (declare (external "receive_delim"))
    (let ((i 0)
          (eof false))
      (while (and (< i size) (not eof))
      (let ((c (uids-channel-input fd)))
        (if (or (= c -1)
                (= (cast char c) (cast char delim)))
            (set eof true)
            (memory-write (+ buf i) c)
            (incr i))))
     (if (= i 0)
        3
        0)))

(defun receive-until-fd (fd buf n delim)
  (declare (external "recvUntil"))
  (let ((z 0:64)
        (eof false))
      (while (and (< z n)
                  (not eof))
        (let ((c (uids-channel-input fd)))
             (memory-write (+ buf z) c)
             (if (or (= (cast char c) (cast char delim))
                     (= (cast char c) (cast char 0xA)))
                (let ((x 0))
                  (set eof true)
                  (incr z))
                (if (= c -1)
                  (set eof true)
                  (incr z))))
        (memory-write (+ buf z) 0:8)
   0)))

(defun receive-until0 (dest n end)
  (declare (external "receive_until0"))
  (let ((z 0:64)
        (eof false))
      (while (and (< z n)
                  (not eof))
        (let ((c (uids-channel-input *uids-standard-input*)))
             (memory-write (+ dest z) c)
             (if (or (= (cast char c) (cast char end))
                     (= (cast char c) (cast char 0xA)))
                (let ((x 0:64))
                  (set eof true)
                  (incr z))
                (if (= c (cast char -1))
                  (set eof true)
                  (incr z)))))
        (memory-write (+ dest z) 0:8)
  z))

(defun getline (dest n)
  (declare (external "getline"))
  (let ((z 0:64)
        (eof false))
      (while (and (< z n)
                  (not eof))
        (let ((c (uids-channel-input *uids-standard-input*)))
             (memory-write (+ dest z) c)
             (if (or (= (cast char c) (cast char 0xA))
                     (= c (cast char -1)))
                (set eof true)
		(incr z))))
        (memory-write (+ dest z) 0:8)
  z))

(defun receive-until (dest n end bytes-read)
  (declare (external "receive_until"))
  (let ((z 0:64)
        (eof false))
      (while (and (< z n)
                  (not eof))
        (let ((c (uids-channel-input *uids-standard-input*)))
             (memory-write (+ dest z) c)
             (if (or (= (cast char c) (cast char end))
                     (= (cast char c) (cast char 0xA)))
                (let ((x 0))
                  (set eof true)
                  (incr z))
                (if (= c -1)
                  (set eof true)
                  (incr z))))
        (memory-write (+ dest z) 0:8)
        (write-word ptr_t (cast ptr_t bytes-read) z)
   0)))

(defun cgc-print (buf)
  (declare (external "print"))
  (puts buf)
  (uids-channel-flush *uids-standard-output*))

(defun uids-tmpfile ()
  (declare (external "tmpfile"))
  (let ((r (uids-channel-open-tmpfile)))
    ;; (uids-ocaml-debug r)
    r))

(defun uids-pipe (buf)
  (declare (external "pipe"))
  (let ((fd (uids-channel-open-pipe)))
    (write-word ptr_t buf fd)
    (write-word ptr_t (ptr+1 ptr_t buf) fd)
    fd))