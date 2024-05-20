(in-package posix)

(defun epoll-create (x)
  (declare (external "epoll_create"))
  ;; (upc-ocaml-debug 0xccccd)
  (upc-channel-open-epoll))

(defun epoll-wait (epfd events nevents timeout)
    (declare (external "epoll_wait"))
    (upc-channel-epoll-wait events nevents))

(defun epoll-ctl (epfd op fd event)
    (declare (external "epoll_ctl"))
    (let ((event-size 12)
          (buf (malloc event-size)))
        (memcpy buf event event-size)
        (upc-channel-epoll-ctl fd buf)))
