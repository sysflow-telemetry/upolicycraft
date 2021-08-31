(in-package posix)

(require memory)
(require string)

;; Assumes flags has MSG_PEEK set.
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
    (uids-channel-seek stream1 pos)))

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

(defun ftell (stream)
  (declare (external "ftell"))
  (uids-channel-offset stream))

(defun fseek (stream offset whence)
  (declare (external "fseek"))
  (uids-channel-seek stream offset))

(defun uids-printf (fmt)
  (declare (external "printf"))
  (puts fmt)
  (uids-channel-flush *uids-standard-output*)
  0)

(defun snprintf (s sz fmt)
   (declare (external "snprintf"))
   (let ((m (malloc sz)))
     (uids-ocaml-snprintf s sz fmt)
   0))

(defun sprintf (s fmt)
   (declare (external "sprintf"))
   (uids-ocaml-sprintf s fmt)
   (strlen s))

(defun uids-scanf (fmt a)
  (declare (external "__isoc99_scanf"))
   (let ((x (uids-ocaml-scanf fmt)))
      (write-word ptr_t a x)
   0))

(defun ferror (fp)
    (declare (external "ferror"))
    0)