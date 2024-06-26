(in-package posix)
(declare (visibility :private))

(require types)
(require ascii)

(defmacro skip-all (pred s)
  (while (pred (memory-read s)) (incr s)))

(defun atoi-prefix (s)
  (or (ascii-is-special s) (ascii-is-whitespace s)))

(defun atoi-read-digit (s)
  (cast ptr_t (- (memory-read s) ?0)))

(defun read-ascii-word (s)
  (skip-all atoi-prefix s)
  (let ((v 0)
        (sign (ascii-sign (memory-read s))))
    (if (= sign 0)
        (set sign 1)
      (incr s))
    (while (ascii-is-digit (memory-read s))
      (set v (+ (* v 10) (atoi-read-digit s)))
      (incr s))
    (* sign v)))

(defmacro make-converter (type s)
  (cast type (read-ascii-word s)))

  ;; (declare (visibility :public) (external "atoi"))

(defun atoi  (s)
  (make-converter int s))

(defun atol (s)
  (declare (visibility :public) (external "atol"))
  (make-converter long s))

(defun atoll (s)
  (declare (visibility :public) (external "atoll"))
  (make-converter long-long s))
