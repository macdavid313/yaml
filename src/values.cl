;;;; values.cl
(in-package #:yaml)

(defstruct (yaml-mark (:constructor mk-yaml-mark))
  (index 0 :type fixnum)
  (line 0 :type fixnum)
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

(def-foreign-call (.strtod. "strtod") ((str :foreign-address)
                                       (endptr :foreign-address))
  :returning :double
  :strings-convert nil
  :call-direct t
  :arg-checking nil)

(defun strtod (str)
  (declare (type simple-string str)
           (optimize (speed 3) (safety 0) (space 0)))
  (let ((*read-default-float-format* 'double-float))
    (the double-float (read-from-string str))))

(define-compiler-macro strtod (str &whole forms)
  (if* (get-entry-point "strtod")
     then (let ((s (gensym "s")))
            `(with-native-string (,s ,str)
               (.strtod. ,s 0)))
     else forms))

(defun yaml-scalar-to-lisp (value tag style)
  (declare (type simple-string value tag)
           (type symbol style)
           (optimize (speed 3) (safety 1)))
  (cond
    ;; Quoted string
    ((member style '(YAML_SINGLE_QUOTED_SCALAR_STYLE YAML_DOUBLE_QUOTED_SCALAR_STYLE) :test 'eq)
     value)
    ;; Null
    ((member value '("null" "Null" "NULL" "~") :test 'string=)
     *yaml-null*)
    ;; True
    ((member value '("true" "True" "TRUE") :test 'string=)
     t)
    ;; False
    ((member value '("false" "False" "FALSE") :test 'string=)
     nil)
    ;; Integer
    ((match-re "^([-+]?[0-9]+)$" value :return :index)
     (multiple-value-bind (res pos) (parse-integer value :junk-allowed nil)
       (declare (ignore pos))
       res))
    ;; Octal digits
    ((match-re "^0o([0-7]+)$" value :return :index)
     (multiple-value-bind (res pos) (parse-integer value :start 2 :radix 8 :junk-allowed nil)
       (declare (ignore pos))
       res))
    ;; Hex digits
    ((match-re "^0x([0-9a-fA-F]+)$" value :return :index)
     (multiple-value-bind (res pos) (parse-integer value :start 2 :radix 16 :junk-allowed nil)
       (declare (ignore pos))
       res))
    ;; Floating-point number
    ((match-re "^[-+]?(\\.[0-9]+|[0-9]+(\\.[0-9]*)?)([eE][-+]?[0-9]+)?$" value :return :index)
     (strtod value))
    ;; NaN
    ((match-re "^\\.(nan|NaN|NAN)$" value :return :index)
     *nan-double*)
    ;; +Inf
    ((match-re "^[+]?(\\.inf|\\.Inf|\\.INF)$" value :return :index)
     *infinity-double*)
    ;; -Inf
    ((match-re "^-(\\.inf|\\.Inf|\\.INF)$" value :return :index)
     *negative-infinity-double*)
    ;; Just a string
    (t value)))
