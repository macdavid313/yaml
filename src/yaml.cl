;;;; yaml.cl
(in-package #:yaml)

;;; Conditions
(define-condition yaml-error (error)
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

;;; Internal implementation
(defmacro with-libyaml-parser ((parser-var) &body body)
  `(with-stack-fobjects ((,parser-var 'yaml_parser_t))
     (when (zerop (yaml_parser_initialize ,parser-var))
       (error 'libyaml-error :from 'yaml_parser_initialize))
     (unwind-protect (progn ,@body)
       (yaml_parser_delete ,parser-var))))

(defconstant +yaml-empty-document+ '.YAML-EMPTY-DOCUMENT.
  "A singleton that represents an empty YAML document.")

(defun yaml-empty-document-p (doc)
  (eq doc +yaml-empty-document+))

(defun load-yaml-document (parser)
  (with-stack-fobjects ((document 'yaml_document_t))
    (when (= 0 (yaml_parser_load parser document))
      (signal-yaml-parser-error parser))
    (let ((root (yaml_document_get_root_node document)))
      (if* (= root 0)
         then +yaml-empty-document+
         else (unwind-protect (load-yaml-node root document)
                (yaml_document_delete document))))))

(defmacro load-yaml-document* (parser)
  (let ((doc (gensym "document-")))
    `(loop for ,doc = (load-yaml-document ,parser)
           until (yaml-empty-document-p ,doc)
           collect ,doc)))

(defun load-yaml-node (node document)
  (case (svref *enum-yaml-node-type* (fslot-value-typed 'yaml_node_t :c node 'type))
    (YAML_SCALAR_NODE   (load-yaml-scalar-node node))
    (YAML_SEQUENCE_NODE (load-yaml-sequence-node node document))
    (YAML_MAPPING_NODE  (load-yaml-mapping-node node document))))

(defun yaml-node-tag (node)
  (let ((tag (fslot-value-typed 'yaml_node_t :c node 'tag)))
    (if (= 0 tag) nil (native-to-string tag))))

(defun yaml-node-style (node node-type)
  (svref (case node-type
           (scalar   *enum-yaml-scalar-style*)
           (sequence *enum-yaml-sequence-style*)
           (mapping  *enum-yaml-mapping-style*))
         (fslot-value-typed 'yaml_node_t :c node 'data node-type 'style)))

(defun yaml-node-scalar (node)
  (native-to-string (fslot-value-typed 'yaml_node_t :c node 'data 'scalar 'value)
                    :length (fslot-value-typed 'yaml_node_t :c node 'data 'scalar 'length)))

(defun load-yaml-scalar-node (node)
  (convert-yaml-scalar (yaml-node-scalar node)
                       (yaml-node-tag node)
                       (yaml-node-style node 'scalar)))

(defun load-yaml-sequence-node (node document)
  (loop with tag = (yaml-node-tag node)
        with style = (yaml-node-style node 'sequence)
        with start = (fslot-value-typed 'yaml_node_t :c node 'data 'sequence 'items 'start)
        with top   = (fslot-value-typed 'yaml_node_t :c node 'data 'sequence 'items 'top)
        with count of-type fixnum = (/ (- top start) #.(sizeof-fobject 'yaml_node_item_t))
        with sequence = (make-array count :element-type t)
        for i from 0
        for item = start then (+ item #.(sizeof-fobject 'yaml_node_item_t))
        for node = (yaml_document_get_node document (fslot-value-typed 'yaml_node_item_t :c item))
        until (= item top)
        do (setf (aref sequence i) (load-yaml-node node document))
        finally (return (convert-yaml-sequence sequence tag style))))

(defun load-yaml-mapping-node (node document)
  (loop with tag = (yaml-node-tag node)
        with style = (yaml-node-style node 'mapping)
        with start = (fslot-value-typed 'yaml_node_t :c node 'data 'sequence 'items 'start)
        with top   = (fslot-value-typed 'yaml_node_t :c node 'data 'sequence 'items 'top)
        with mapping = (make-hash-table :test 'equal)
        for pair = start then (+ pair #.(sizeof-fobject 'yaml_node_pair_t))
        for node-k = (yaml_document_get_node document (fslot-value-typed 'yaml_node_pair_t :c pair 'key))
        for node-v = (yaml_document_get_node document (fslot-value-typed 'yaml_node_pair_t :c pair 'value))
        until (= pair top)
        do (setf (gethash (load-yaml-node node-k document) mapping)
                 (load-yaml-node node-v document))
        finally (return (convert-yaml-mapping (yaml-mapping-merge-maybe mapping) tag style))))

(defun yaml-mapping-merge-maybe (mapping)
  (multiple-value-bind (mapping-to-merge exists-p) (gethash "<<" mapping)
    (when exists-p
      (check-type mapping-to-merge hash-table)
      (remhash "<<" mapping)
      (setq mapping-to-merge (yaml-mapping-merge-maybe mapping-to-merge))
      (let (more? key-to-merge new-val)
        (with-hash-table-iterator (gen mapping-to-merge)
          (multiple-value-setq (more? key-to-merge new-val) (gen))
          (while more?
            (setf (gethash key-to-merge mapping) new-val)
            (multiple-value-setq (more? key-to-merge new-val) (gen)))))))
  mapping)

(defun .read-yaml-string. (str multiple)
  (with-libyaml-parser (parser)
    (with-native-string (input str :native-length-var len)
      (yaml_parser_set_input_string parser input len)
      (if* multiple
         then (load-yaml-document* parser)
         else (load-yaml-document  parser)))))

(defun .read-yaml-file. (filespec multiple)
  (.read-yaml-string. (file-contents filespec) multiple))

(define-compiler-macro .read-yaml-file. (filespec multiple &whole form)
  (if* (and (get-entry-point "fopen") (get-entry-point "fclose"))
     then (let ((pathname (gensym "pathname"))
                (fp (gensym "fp"))
                (parser (gensym "parser")))
            `(let* ((,pathname (etypecase ,filespec
                                 (string (pathname ,filespec))
                                 (pathname ,filespec)))
                    (,fp (fopen (namestring ,pathname) "r")))
               (if* (zerop ,fp)
                  then (.read-yaml-string. (file-contents ,filespec) ,multiple)
                  else (unwind-protect (with-libyaml-parser (,parser)
                                         (yaml_parser_set_input_file ,parser ,fp)
                                         (if* ,multiple
                                            then (load-yaml-document* ,parser)
                                            else (load-yaml-document  ,parser)))
                         (assert (zerop (fclose ,fp)))))))
     else form))

;;; Top-level APIs
(defun read-yaml (input &key pathname-p multiple)
  (if* pathname-p
     then (.read-yaml-file.   input multiple)
     else (.read-yaml-string. input multiple)))

;;; load libyaml shared library
(eval-when (:load-toplevel)
  (let ((workdir (directory-namestring *load-pathname*)))
    (load #.(string+ "libyaml" #\. (car *load-foreign-types*))
          :foreign t
          :search-list (list workdir    ; the distributed libyaml shared library
                             (string+ workdir "../") ; for development environment
                             ))))
