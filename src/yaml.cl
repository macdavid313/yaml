;;;; yaml.cl
(in-package #:yaml)

(eval-when (:load-toplevel)
  (let ((workdir (directory-namestring *load-pathname*)))
    (load #.(string+ "libyaml" #\. (car *load-foreign-types*))
          :foreign t
          :search-list (list workdir    ; the distributed libyaml shared library
                             (string+ workdir "../") ; for development environment
                             ))))

(def-foreign-call fopen ((pathname (* :char) simple-string)
                         (mode (* :char) simple-string))
                  :returning :foreign-address
                  :strings-convert t
                  :arg-checking nil)

(def-foreign-call fclose ((fp :foreign-address))
  :returning :int
  :call-direct t
  :arg-checking nil)

(defmacro with-libyaml-parser ((parser-var) &body body)
  `(with-stack-fobjects ((,parser-var 'yaml_parser_t))
     (when (zerop (yaml_parser_initialize ,parser-var))
       (error 'libyaml-error :from 'yaml_parser_initialize))
     (unwind-protect (progn ,@body)
       (yaml_parser_delete ,parser-var))))

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

(defun load-yaml-node (node document)
  (case (svref *enum-yaml-node-type* (fslot-value-typed 'yaml_node_t :c node 'type))
    (YAML_SCALAR_NODE   (load-yaml-scalar-node node))
    (YAML_SEQUENCE_NODE (load-yaml-sequence-node node document))
    (YAML_MAPPING_NODE  (load-yaml-mapping-node node document))))

(defun load-yaml-scalar-node (node)
  (let ((tag (fslot-value-typed 'yaml_node_t :c node 'tag))
        (value (native-to-string (fslot-value-typed 'yaml_node_t :c node 'data 'scalar 'value)
                                 :length (fslot-value-typed 'yaml_node_t :c node 'data 'scalar 'length)))
        (style (svref *enum-yaml-scalar-style*
                      (fslot-value-typed 'yaml_node_t :c node 'data 'scalar 'style))))
    (setq tag (if (= 0 tag) nil (native-to-string tag)))
    (yaml-scalar->lisp value tag style)))

(defun load-yaml-sequence-node (node document)
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
          do (setf (aref sequence i) (load-yaml-node node document))
          finally (return sequence))))

(defun load-yaml-mapping-node (node document)
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
          do (setf (gethash (load-yaml-node k document) mapping)
                   (load-yaml-node v document))
          finally (return mapping))))

(defun load-yaml (parser)
  (with-stack-fobjects ((document 'yaml_document_t))
    (when (= 0 (yaml_parser_load parser document))
      (yaml_document_delete document)
      (signal-yaml-parser-error parser))
    (let ((root (yaml_document_get_root_node document)))
      (if* (= root 0)
         then (yaml_document_delete document)
              'YAML_EMPTY_DOCUMENT
         else (unwind-protect (load-yaml-node root document)
                (yaml_document_delete document))))))

;;; Top-level APIs
(defgeneric read-yaml (input)
  (:documentation "Read a YAML document from `input'."))

(defmethod read-yaml ((str string))
  (with-libyaml-parser (parser)
    (with-native-string (input str :native-length-var size)
      (yaml_parser_set_input_string parser input size)
      (load-yaml parser))))

(defmethod read-yaml ((path pathname))
  (if* (and (get-entry-point "fopen") (get-entry-point "fclose"))
     then (let ((fp (fopen (namestring path) "r")))
            (if* (zerop fp)
               then (read-yaml (file-contents path))
               else (unwind-protect
                         (with-libyaml-parser (parser)
                           (yaml_parser_set_input_file parser fp)
                           (load-yaml parser))
                      (assert (zerop (fclose fp))))))
     else (read-yaml (file-contents path))))

(defmacro read-yaml-documents (parser)
  (let ((doc (gensym "doc-")))
    `(loop for ,doc = (load-yaml ,parser)
           until (eq ,doc 'YAML_EMPTY_DOCUMENT)
           collect ,doc)))

(defgeneric read-yaml* (input)
  (:documentation "Read YAML document(s) into a list from `input'"))

(defmethod read-yaml* ((str string))
  (with-libyaml-parser (parser)
    (with-native-string (input str :native-length-var size)
      (yaml_parser_set_input_string parser input size)
      (read-yaml-documents parser))))

(defmethod read-yaml* ((path pathname))
  (if* (and (get-entry-point "fopen") (get-entry-point "fclose"))
     then (let ((fp (fopen (namestring path) "r")))
            (if* (zerop fp)
               then (read-yaml (file-contents path))
               else (unwind-protect
                         (with-libyaml-parser (parser)
                           (yaml_parser_set_input_file parser fp)
                           (read-yaml* parser))
                      (assert (zerop (fclose fp))))))
     else (read-yaml* (file-contents path))))
