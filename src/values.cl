;;;; values.cl
(in-package #:yaml)

(defstruct (yaml-mark (:constructor mk-yaml-mark))
  (index  0 :type fixnum)
  (line   0 :type fixnum)
  (column 0 :type fixnum))

(defun make-yaml-mark (ptr)
  (mk-yaml-mark :index  (fslot-value-typed 'yaml_mark_t :c ptr 'index)
                :line   (fslot-value-typed 'yaml_mark_t :c ptr 'line)
                :column (fslot-value-typed 'yaml_mark_t :c ptr 'column)))

(defstruct yaml-version-directive
  (major 0 :type fixnum :read-only t)
  (minor 0 :type fixnum :read-only t))

(defstruct yaml-tag-directive
  (handle "" :type simple-string :read-only t)
  (prefix "" :type simple-string :read-only t))

(defparameter *yaml-null* nil
  "A special variable that determines the value that corresponds to a YAML
\"null\" (or empty scalar) value. It is nil by default.")

(defun yaml-null-p (x)
  (eq x *yaml-null*))

(defun convert-yaml-scalar (scalar tag style)
  (declare (type simple-string scalar)
           (type (or simple-string nil) tag)
           (type symbol style)
           (ignorable tag style)
           (optimize (speed 3) (safety 1)))
  (cond
   ;; Quoted string
   ((member style '(YAML_SINGLE_QUOTED_SCALAR_STYLE YAML_DOUBLE_QUOTED_SCALAR_STYLE) :test 'eq)
    scalar)
   ;; Null
   ((member scalar '("null" "Null" "NULL" "~") :test 'string=)
    *yaml-null*)
   ;; True
   ((member scalar '("true" "True" "TRUE") :test 'string=)
    t)
   ;; False
   ((member scalar '("false" "False" "FALSE") :test 'string=)
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

(defun convert-yaml-sequence (sequence tag style)
  (declare (type yaml-sequence sequence)
           (type (or simple-string nil) tag)
           (type symbol style)
           (ignorable tag style)
           (optimize (speed 3) (safety 1)))
  sequence)

(defun convert-yaml-mapping (mapping tag style)
  (declare (type yaml-mapping mapping)
           (type (or simple-string nil) tag)
           (type symbol style)
           (ignorable tag style)
           (optimize (speed 3) (safety 1)))
  mapping)
