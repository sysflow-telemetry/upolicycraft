(in-package posix)

(defun uids-strcspn (str set)
  (declare (external "strcspn"))
  (let ((len 0))
    (until (or (points-to-null str)
               (= (cast int (memory-read str))
                  (cast int (memory-read set))))
      (incr str len))
    len))

(defun strtok_r (str sep ptr)
  (declare (external "strtok_r"))
  (when str (write-word ptr_t ptr str))
  (let ((str (read-word ptr_t ptr))
        (del (+ str (uids-strcspn str sep)))
        (next (if (points-to-null del) del (+ del 1))))
      (if (points-to-null str) nil
         (memory-write del 0)
         (write-word ptr_t ptr next)
         str)))

(defparameter *strtok-static-storage* 0)

(defun strtok (str sep)
  (declare (external "strtok"))
  (when (= *strtok-static-storage* 0)
    (set *strtok-static-storage* (malloc (sizeof ptr_t))))
    (strtok_r str sep *strtok-static-storage*))

(defun strerror (errnum)
  (declare (external "strerror"))
  0)
