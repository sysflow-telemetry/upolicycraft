(in-package posix)

(defun htons (v)
  (declare (external "htons"))
  v)

(defun uids-inet-aton (str inp)
  (declare (external "inet_aton"))
  (let ((localhost 0x100007f))
    (write-word ptr_t inp localhost)
    localhost))

(defun uids-inet-ntoa (inp)
  (declare (external "inet_ntoa"))
  (let ((buf (malloc 16)))
      ;; (uids-ocaml-debug 0xdeadc00de)
      ;; (write-word ptr_t fname 0x3e74656e3c) ;; <net>
      (write-word char buf 0x73)
      (write-word char (+ buf 1) 0x72)
      (write-word char (+ buf 2) 0x76)
      (write-word char (+ buf 3) 0x0)
      ;; (puts fname)
      buf))
