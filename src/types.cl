;;;; values.cl
(in-package #:yaml)

(defparameter *yaml-null* nil
  "A special variable that determines the value that corresponds to a YAML
\"null\" (or empty scalar) value. It is nil by default.")

(defun yaml-null-p (x)
  (eq x *yaml-null*))

(defconstant +yaml-true-values+
  '("y" "Y" "yes" "Yes" "true" "True" "TRUE" "on" "On" "ON"))

(defconstant +yaml-false-values+
  '("n" "N" "no" "No" "NO" "false" "False" "False" "off" "Off" "OFF"))

(defun convert-yaml-scalar (scalar tag style)
  (declare (type simple-string scalar)
           (type (or simple-string nil) tag)
           (type symbol style)
           (ignorable tag)
           (optimize (speed 3) (safety 1)))
  (cond
    ;; Quoted string
    ((member style '(YAML_SINGLE_QUOTED_SCALAR_STYLE YAML_DOUBLE_QUOTED_SCALAR_STYLE) :test 'eq)
     scalar)
    ;; Null
    ((member scalar '("null" "Null" "NULL" "~") :test 'string=)
     *yaml-null*)
    ;; True
    ((member scalar +yaml-true-values+ :test 'string=)
     t)
    ;; False
    ((member scalar +yaml-false-values+ :test 'string=)
     nil)
    ;; Integer
    ((match-re "^([-+]?[0-9]+)$" scalar :return :index)
     (multiple-value-bind (res pos) (parse-integer scalar :junk-allowed nil)
       (declare (ignore pos))
       res))
    ;; Octal digits
    ((match-re "^0o([0-7]+)$" scalar :return :index)
     (multiple-value-bind (res pos) (parse-integer scalar :start 2 :radix 8 :junk-allowed nil)
       (declare (ignore pos))
       res))
    ;; Hex digits
    ((match-re "^0x([0-9a-fA-F]+)$" scalar :return :index)
     (multiple-value-bind (res pos) (parse-integer scalar :start 2 :radix 16 :junk-allowed nil)
       (declare (ignore pos))
       res))
    ;; Floating-point number
    ((match-re "^[-+]?(\\.[0-9]+|[0-9]+(\\.[0-9]*)?)([eE][-+]?[0-9]+)?$" scalar :return :index)
     (strtod scalar))
    ;; NaN
    ((match-re "^\\.(nan|NaN|NAN)$" scalar :return :index)
     *nan-double*)
    ;; +Inf
    ((match-re "^[+]?(\\.inf|\\.Inf|\\.INF)$" scalar :return :index)
     *infinity-double*)
    ;; -Inf
    ((match-re "^-(\\.inf|\\.Inf|\\.INF)$" scalar :return :index)
     *negative-infinity-double*)
    ;; Just a string
    (t scalar)))

(deftype yaml-sequence () 'simple-vector)

(defun convert-yaml-sequence (sequence tag style)
  (declare (type yaml-sequence sequence)
           (type (or simple-string nil) tag)
           (type symbol style)
           (ignorable tag style)
           (optimize (speed 3) (safety 1)))
  sequence)

(deftype yaml-mapping () 'hash-table)

(defun convert-yaml-mapping (mapping tag style)
  (declare (type yaml-mapping mapping)
           (type (or simple-string nil) tag)
           (type symbol style)
           (ignorable tag style)
           (optimize (speed 3) (safety 1)))
  mapping)
