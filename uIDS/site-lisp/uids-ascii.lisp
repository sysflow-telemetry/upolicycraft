(in-package posix)

(defun ascii-is-digit (s)
  "(ascii-is-digit s) is true if S is an ascii representation of decimal digit"
  (declare (external "isdigit"))
  (and (>= s ?0) (<= s ?9)))

(defun ascii-is-alpha (c)
  (declare (external "isalpha"))
  (or (and (>= c 0x41:8) (<= c 0x5A:8))
      (and (>= c 0x61:8) (<= c 0x7A:8))))

  ;; (< (- (logor c 32:8) ?a) 26))

(defun ascii-is-upper (c)
  (declare (external "isupper"))
  (< (- (cast char c) ?A) 26))

(defun ascii-is-lower (c)
  (declare (external "islower"))
  (< (- (cast char c) ?a) 26))

(defun ascii-is-print (c)
  (declare (external "isprint"))
  ;; (and (> c 0x1f) (not (= 0x7f))))
  (and (> c 0x1f:8) (not (= c 0x7f:8))))

(defun ascii-to-lower (c)
  (declare (external "tolower"))
  (if (ascii-is-upper c) (logor (cast char c) 32:8) c))

(defun ascii-to-upper (c)
  (declare (external "toupper"))
  (if (ascii-is-lower (cast char c)) (logand c 0x5f) c))

(defun ascii-is-alphanum (c)
  (declare (external "isalnum"))
  (uids-ocaml-debug 0xfabc0de)
  (or (ascii-is-alpha c)
      (ascii-is-digit c)))
