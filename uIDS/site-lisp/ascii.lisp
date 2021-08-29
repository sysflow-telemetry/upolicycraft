(in-package posix)

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
