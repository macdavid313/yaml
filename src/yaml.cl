;;;; yaml.cl
(in-package #:yaml)

;;; Utilities
(def-foreign-call fopen ((pathname (* :char) simple-string)
                         (mode (* :char) simple-string))
  :returning :foreign-address
  :strings-convert t
  :arg-checking nil)

(def-foreign-call fclose ((fp :foreign-address))
  :returning :int
  :call-direct t
  :arg-checking nil)

(def-foreign-call strtod ((str :foreign-address)
                          (endptr :foreign-address))
  :returning :double
  :strings-convert nil
  :call-direct t
  :arg-checking nil)

(defmacro with-libyaml-parser ((parser-var) &body body)
  `(with-stack-fobjects ((,parser-var 'yaml_parser_t))
     (when (zerop (yaml_parser_initialize ,parser-var))
       (error 'libyaml-error :from 'yaml_parser_initialize))
     (unwind-protect (progn ,@body)
       (yaml_parser_delete ,parser-var))))

(defun libyaml-parser-state (parser)
  (svref *enum-yaml-parser-state*
         (fslot-value-typed 'yaml_parser_t :foreign parser 'state)))

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

;;; Conditions
(define-condition yaml-error ()
  ()
  (:documentation "The base class of all YAML conditions."))

(define-condition libyaml-error (yaml-error)
  ((from :reader libyaml-error-from
         :initarg :from
         :type symbol
         :documentation "The C function from libyaml that causes the error."))
  (:report
   (lambda (condition stream)
     (format stream "Error encountered whiling calling '~a"
             (libyaml-error-from condition)))))

(define-condition yaml-parser-error (yaml-error)
  ((message :reader yaml-parser-error-message
            :initarg :message
            :type simple-string
            :documentation "The error message (see `problem' field of `yaml_parser_t').")
   (mark :reader yaml-error-mark
         :initarg :mark
         :type yaml-mark
         :documentation "The mark (location) where the error happened"))
  (:report
   (lambda (condition stream)
     (format stream "YAML parser error at line ~A, column ~A: ~A.~&"
             (yaml-mark-line (yaml-error-mark condition))
             (yaml-mark-column (yaml-error-mark condition))
             (yaml-parser-error-message condition)))))

(defun signal-yaml-parser-error (parser)
  (let ((problem (fslot-value-typed 'yaml_parser_t :foreign parser 'problem))
        (problem_mark (fslot-value-typed 'yaml_parser_t :foreign parser 'problem_mark)))
    (error 'yaml-parser-error :message (native-to-string problem)
                              :mark (make-yaml-mark problem_mark))))

;;; Internal implementations
(defun yaml-scalar->lisp (value tag style)
  (declare (ignorable tag)
           (type simple-string value)
           (type symbol style)
           (optimize (speed 3) (safety 0) (space 0)))
  (cond
    ;; Quoted string
    ((member style '(YAML_SINGLE_QUOTED_SCALAR_STYLE YAML_DOUBLE_QUOTED_SCALAR_STYLE) :test 'eq)
     value)
    ;; Null
    ((member value '("null" "Null" "NULL" "~") :test 'equal)
     nil)
    ;; True
    ((member value '("true" "True" "TRUE") :test 'equal)
     t)
    ;; False
    ((member value '("false" "False" "FALSE") :test 'equal)
     nil)
    ;; Integer
    ((match-re "^([-+]?[0-9]+)$" value :single-line t)
     (parse-integer value :junk-allowed nil))
    ;; Octal digits
    ((match-re "^0o([0-7]+)$" value :single-line t)
     (parse-integer value :start 2 :radix 8 :junk-allowed nil))
    ;; Hex digits
    ((match-re "^0x([0-9a-fA-F]+)$" value :single-line t)
     (parse-integer value :start 2 :radix 16 :junk-allowed nil))
    ;; Floating-point number
    ((match-re "^[-+]?(\\.[0-9]+|[0-9]+(\\.[0-9]*)?)([eE][-+]?[0-9]+)?$" value :single-line t)
     (if* (get-entry-point "strtod")
        then (with-native-string (str value)
               (strtod str 0))
        else (let ((*read-default-float-format* 'double-float))
               (read-from-string value))))
    ;; NaN
    ((member value '(".nan" ".NaN" ".NAN") :test 'equal)
     *nan-double*)
    ;; +Inf
    ((match-re "^[+]?(\\.inf|\\.Inf|\\.INF)$" value :single-line t)
     *infinity-double*)
    ;; -Inf
    ((match-re "^-(\\.inf|\\.Inf|\\.INF)$" value :single-line t)
     *negative-infinity-double*)
    ;; Just a string
    (t value)))

;;; Top-level APIs
(defun read-yaml-node (node document)
  (case (svref *enum-yaml-node-type* (fslot-value-typed 'yaml_node_t :c node 'type))
    (YAML_SCALAR_NODE   (read-yaml-scalar-node node))
    (YAML_SEQUENCE_NODE (read-yaml-sequence-node node document))
    (YAML_MAPPING_NODE  (read-yaml-mapping-node node document))))

(defun read-yaml-scalar-node (node)
  (let ((tag (fslot-value-typed 'yaml_node_t :c node 'tag))
        (value (native-to-string (fslot-value-typed 'yaml_node_t :c node 'data 'scalar 'value)
                                 :length (fslot-value-typed 'yaml_node_t :c node 'data 'scalar 'length)))
        (style (svref *enum-yaml-scalar-style*
                      (fslot-value-typed 'yaml_node_t :c node 'data 'scalar 'style))))
    (setq tag (if (= 0 tag) nil (native-to-string tag)))
    (yaml-scalar->lisp value tag style)))

(defun read-yaml-sequence-node (node document)
  (let ((tag   (fslot-value-typed 'yaml_node_t :c node 'tag))
        (start (fslot-value-typed 'yaml_node_t :c node 'data 'sequence 'items 'start))
        (top   (fslot-value-typed 'yaml_node_t :c node 'data 'sequence 'items 'top))
        (style (fslot-value-typed 'yaml_node_t :c node 'data 'sequence 'style)))
    (setq tag (if (= 0 tag) nil (native-to-string tag))
          style (svref *enum-yaml-sequence-style* style))
    (loop with sequence = (make-array (/ (- top start) #.(sizeof-fobject 'yaml_node_item_t))
                                      :element-type t)
          for i from 0
          for item = start then (+ item #.(sizeof-fobject 'yaml_node_item_t))
          for node = (yaml_document_get_node document (fslot-value-typed 'yaml_node_item_t :c item))
          until (= item top)
          do (setf (aref sequence i) (read-yaml-node node document))
          finally (return sequence))))

(defun read-yaml-mapping-node (node document)
  (let ((tag   (fslot-value-typed 'yaml_node_t :c node 'tag))
        (start (fslot-value-typed 'yaml_node_t :c node 'data 'mapping 'pairs 'start))
        (top   (fslot-value-typed 'yaml_node_t :c node 'data 'mapping 'pairs 'top))
        (style (fslot-value-typed 'yaml_node_t :c node 'data 'mapping 'style)))
    (setq tag (if (= 0 tag) nil (native-to-string tag))
          style (svref *enum-yaml-mapping-style* style))
    (loop with mapping = (make-hash-table :test 'equal
                                          :rehash-threshold 1
                                          :size (/ (- top start) #.(sizeof-fobject 'yaml_node_pair_t)))
          for pair = start then (+ pair #.(sizeof-fobject 'yaml_node_pair_t))
          for k = (yaml_document_get_node document (fslot-value-typed 'yaml_node_pair_t :c pair 'key))
          for v = (yaml_document_get_node document (fslot-value-typed 'yaml_node_pair_t :c pair 'value))
          until (= pair top)
          do (setf (gethash (read-yaml-node k document) mapping)
                   (read-yaml-node v document))
          finally (return mapping))))

(defun read-yaml (parser)
  (with-stack-fobjects ((document 'yaml_document_t))
    (when (= 0 (yaml_parser_load parser document))
      (yaml_document_delete document)
      (signal-yaml-parser-error parser))
    (let ((root (yaml_document_get_root_node document)))
      (if* (= root 0)
         then (yaml_document_delete document)
              'YAML_EMPTY_DOCUMENT
         else (unwind-protect (read-yaml-node root document)
                (yaml_document_delete document))))))

(defun parse-yaml-string (str)
  (with-libyaml-parser (parser)
    (with-native-string (input str :native-length-var size)
      (yaml_parser_set_input_string parser input size)
      (read-yaml parser))))

(defun parse-yaml-file (filespec)
  (parse-yaml-string (file-contents filespec)))

(define-compiler-macro parse-yaml-file (filespec &whole forms)
  (if* (and (get-entry-point "fopen") (get-entry-point "fclose"))
     then (let ((fp (gensym "fp"))
                (parser (gensym "parser")))
            `(progn
               (assert (probe-file ,filespec))
               (let ((,fp (fopen (namestring (if (pathnamep ,filespec) filespec (pathname ,filespec)))
                                 "r")))
                 (if* (zerop ,fp)
                    then (parse-yaml-string (file-contents ,filespec))
                    else (unwind-protect
                              (with-libyaml-parser (,parser)
                                (yaml_parser_set_input_file ,parser ,fp)
                                (read-yaml ,parser))
                           (assert (zerop (fclose ,fp))))))))
     else forms))

(defmacro parse-yaml* (parser)
  (let ((doc (gensym "doc-")))
    `(loop for ,doc = (read-yaml ,parser)
           until (eq ,doc 'YAML_EMPTY_DOCUMENT)
           collect ,doc)))

(defun parse-yaml-string* (str)
  (with-libyaml-parser (parser)
    (with-native-string (input str :native-length-var size)
      (yaml_parser_set_input_string parser input size)
      (parse-yaml* parser))))

(defun parse-yaml-file* (filespec)
  (parse-yaml-string* (file-contents filespec)))

(define-compiler-macro parse-yaml-file* (filespec &whole forms)
  (if* (and (get-entry-point "fopen") (get-entry-point "fclose"))
     then (let ((fp (gensym "fp"))
                (parser (gensym "parser")))
            `(progn
               (assert (probe-file ,filespec))
               (let ((,fp (fopen (namestring (if (pathnamep ,filespec) filespec (pathname ,filespec)))
                                 "r")))
                 (if* (zerop ,fp)
                    then (parse-yaml-string* (file-contents ,filespec))
                    else (unwind-protect
                              (with-libyaml-parser (,parser)
                                (yaml_parser_set_input_file ,parser ,fp)
                                (parse-yaml* ,parser))
                           (assert (zerop (fclose ,fp))))))))
     else forms))
