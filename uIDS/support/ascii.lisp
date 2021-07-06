(defun ascii-special (s)
  "(ascii-special S) is true if S is an ascii special character"
  (< s 32))

(defun ascii-whitespace (s:8)
  "(ascii-whitespace S)is true if S is a whitespace"
  (or (= s 10:8)
      (= s 13:8)
      (= s 32:8)))

(defun ascii-sign (s:8)
  "(ascii-sign S) is 1 if S is + and -1 otherwise"
  (if (= s ?+) 1 -1))

(defun ascii-digit (s:8)
  "(ascii-digit s) is true if S is an ascii representation of decimal digit"
  (< (- s ?0) 10))

(defun ascii-is-special (s)
  "(ascii-special S) is true if S is an ascii special character"
  (< s 32))

(defun ascii-is-whitespace (s)
  "(ascii-is-whitespace S) is true if S is \t, \n, \r, or SPACE"
  (or (= s 9)
      (= s 10)
      (= s 13)
      (= s 32)))

(defun ascii-sign (s)
  "(ascii-sign S) is 1 if S is +, -1 if it -, or 0 otherwise"
  (case s
    ?- -1
    ?+  1
    0))

(defun ascii-is-digit (s)
  "(ascii-is-digit s) is true if S is an ascii representation of decimal digit"
  (declare (external "isdigit"))
  (< (- s ?0) 10))

(defun ascii-is-alphanum (c)
  (declare (external "isalnum"))
  (uids-ocaml-debug 0xabcd)
  (uids-ocaml-debug c)
  (or (ascii-is-alpha c)
      (ascii-is-digit c)))

;; (ascii-is-digit c)

(defun ascii-is-alpha (c)
  (declare (external "isalpha"))
  (< (- (logor c 32:8) ?a) 26))

(defun ascii-is-upper (c)
  (declare (external "isupper"))
  (< (- c ?A) 26))

(defun ascii-is-lower (c)
  (declare (external "islower"))
  (< (- c ?a) 26))

(defun ascii-is-print (c)
  (declare (external "isprint"))
  ;;(and (> c 0x1f) (not (= 0x7f))))
  (and (> c 0x1f) (not (= c 0x7f))))

(defun ascii-to-lower (c)
  (declare (external "tolower"))
  (if (ascii-is-upper c) (logor c 32) c))

(defun ascii-to-upper (c)
  (declare (external "toupper"))
  (if (ascii-is-lower (cast char c)) (logand c 0x5f) c))