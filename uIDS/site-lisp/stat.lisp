(in-package posix)

(defun uids-fstat (fd buf)
  (declare (external "fstat64" "__fstat"))
  (let ((offset 48))
    ;; (write-word ptr_t (+ buf offset) size)
    (uids-ocaml-fstat fd buf)))

(defun uids-stat (filename buf)
  (declare (external "stat64" "__stat"))
  ;;(uids-ocaml-debug 0xf0bc0de)
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

(defun uids-fchmod (fd mode)
  (declare (external "fchmod"))
  0)

(defun uids-mkdir (path mode)
  (declare (external "mkdir"))
  0)
