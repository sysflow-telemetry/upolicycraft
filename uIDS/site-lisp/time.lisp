(in-package posix)

(require memory-allocator)
(require string)

(defun uids-localtime-r (time tm)
  (declare (external "localtime_r"))
  (memset tm 0 (* (sizeof int) 9)))

(defun uids-localtime (time)
  (declare (external "localtime"))
  (let ((time-struct-size 56)
       (m (malloc time-struct-size)))
  (memset m 0 time-struct-size)))

(defun uids-gmtime (tm)
  (declare (external "gmtime"))
  (uids-localtime tm))

(defun uids-gettimeofday (tv tz)
  (declare (external "gettimeofday"))
  0)
