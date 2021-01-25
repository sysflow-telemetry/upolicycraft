(require libc-init)
(require memory)
(require pointers)
(require types)

(defun fputc (char stream)
  (declare (external "fputc" "putc"))
  (if (= 0 (channel-output stream char)) char -1))

(defun putchar (char)
  (declare (external "putchar"))
  (fputc char *standard-output*))

(defun fputs (p stream)
  (declare (external "fputs"))
  (while (not (points-to-null p))
    (fputc (cast int (memory-read p)) stream)
    (incr p))
  (fputc 0xA stream))

(defun puts (p)
  (declare (external "puts"))
  (fputs p *standard-output*)
  (channel-flush *standard-output*))

(defun put (p)
  (declare (external "put"))
  (fputs p *standard-output*)
  (channel-flush *standard-output*))

;; the channel module have rough equality between streams and
;; file descriptors, as they both are represented as integers. We are currently
;; ignoring modes, we will add them later, of course.
(defun fopen (path mode)
  (declare (external "fopen" "open"))
  (channel-open path))

(defun open3 (path flags mode)
  (declare (external "open"))
  (fopen path mode))

(defun output-item-nth-char (ptr size item fd i)
  (= 0 (channel-output
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
  (let ((i 0))
    (while (and (< i n)
                (= size (output-item buf size i stream)))
      (incr i))
    i))

(defun write (fd buf cnt)
  (declare (external "write"))
  (let ((written (fwrite buf 1 cnt fd))
        (failure (channel-flush fd)))
    (or failure written)))

(defun input-item-nth-char (ptr size item desc i)
  (let ((c (channel-input desc)))
    (if (= c -1) 0
      (memory-write (+ ptr (* size item) i) (cast char c))
      1)))

(defun input-item (buf size item fd)
  (let ((i 0))
    (while (and (< i size)
                (input-item-nth-char buf size item fd i))
      (incr i))
    i))

(defun fread (ptr size n stream)
  (declare (external "fread"))
  (let ((i 0))
    (while (and
            (< i n)
            (= size (input-item ptr size i stream)))
      (incr i))
    i))

(defun read (fd buf n)
  (declare (external "read"))
  (fread buf 1 n fd))

(defun fgetc (stream)
  (declare (external "fgetc" "getc"))
  (channel-input stream))

(defun fgets-step (ptr len str i)
  (let ((c (channel-input str)))
    (if (= c -1) 0
      (memory-write (+ ptr i) (cast char c))
      (not (= c 0xA:8)))))

(defun fgets (ptr len str)
  (declare (external "fgets"))
  (let ((i 0))
    (while (and (< i len)
                (fgets-step ptr len str i))
      (incr i))
    (memory-write (+ ptr (min (-1 len) (+ ptr i))) 0:8)
    ptr))

(defun getchar ()
  (declare (external "getchar"))
  (fgetc *standard-input*))

(defun uids-printf (fmt)
  (declare (external "printf"))
  (puts fmt)
  (channel-flush *standard-output*)
  0)

(defmethod machine-kill ()
  (channel-flush *standard-output*)
  (channel-flush *standard-error*))

(defun transmit (fd buf count tx-bytes)
  (declare (external "transmit"))
  (fwrite buf count 1 fd)
  (write-word int tx-bytes count)
  0)

;; (n (fread buf count 1 fd))

(defun receive-step (ptr len str i)
  (let ((c (channel-input str)))
    (if (= c -1) 0
      (memory-write (+ ptr i) (cast char c)))))

(defun receive (fd buf count rx-bytes)
  (declare (external "receive"))
  (let ((z 0))
    (while (and (< z count)
                (receive-step buf count fd z))
      (incr z))
    (when rx-bytes
      (write-word ptr_t (cast ptr_t rx-bytes) z))
    0))

(defun receive-bytes (buf count)
   (declare (external "receive_bytes"))
   (receive *standard-input* buf count nil))

(defun transmit-all (fd buf size)
  (declare (external "transmit_all"))
  (write fd buf size)
  0)

(defun receive-delim (fd buf size delim)
    (declare (external "receive_delim"))
    (let ((i 0)
          (eof false))
      (while (and (< i size) (not eof))
      (let ((c (channel-input fd)))
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
        (let ((c (channel-input fd)))
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

(defun receive-until (dest n end bytes-read)
  (declare (external "receive_until"))
  (let ((z 0:64)
        (eof false))
      (while (and (< z n)
                  (not eof))
        (let ((c (channel-input *standard-input*)))
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
  (channel-flush *standard-output*))

