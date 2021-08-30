(in-package posix)

(defun epoll-create (x)
  (declare (external "epoll_create"))
  ;; (uids-ocaml-debug 0xccccd)
  1024)

(defun epoll-wait (epfd events nevents timeout)
   (declare (external "epoll_wait"))
   nevents)
