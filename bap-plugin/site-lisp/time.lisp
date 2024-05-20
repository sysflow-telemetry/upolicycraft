(in-package posix)

(require memory-allocator)
(require string)

(defun upc-localtime-r (time tm)
  (declare (external "localtime_r"))
  (memset tm 0 (* (sizeof int) 9)))

(defun upc-localtime (time)
  (declare (external "localtime"))
  (let ((time-struct-size 56)
       (m (malloc time-struct-size)))
  (memset m 0 time-struct-size)))

(defun upc-gmtime (tm)
  (declare (external "gmtime"))
  (upc-localtime tm))

(defun upc-gettimeofday (tv tz)
  (declare (external "gettimeofday"))
  0)
