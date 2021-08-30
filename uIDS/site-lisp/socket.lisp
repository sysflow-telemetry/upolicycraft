(require types)
(in-package posix)

(defmacro open-socket-fd ()
    (let ((fname (malloc 16)))
      ;; (uids-ocaml-debug 0xdeadc00de)
      ;; (write-word ptr_t fname 0x3e74656e3c) ;; <net>
      (write-word char fname 0x73)
      (write-word char (+ fname 1) 0x72)
      (write-word char (+ fname 2) 0x76)
      (write-word char (+ fname 3) 0x0)
      ;; (puts fname)
      (let ((fd (uids-channel-open-network fname)))
        (uids-ocaml-add-socket fd)
        fd))

(defun socketpair (domain type protocol socket-vector)
  (declare (external "socketpair"))
  (let ((fd (open-socket-fd)))
    (write-word int32_t socket-vector fd)
    (write-word int32_t (+ socket-vector (sizeof int32_t)) fd)
    0))

(defun uids-socket (domain tp protocol)
  (declare (external "socket"))
  (open-socket-fd))

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

(defun uids-setsockopt (fd level opt optval len)
  (declare (external "setsockopt"))
  0)

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


