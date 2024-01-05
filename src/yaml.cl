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

(defstruct (yaml-mark (:constructor mk-yaml-mark))
  index
  line
  column)

(defun make-yaml-mark (ptr)
  (mk-yaml-mark :index  (fslot-value-typed 'yaml_mark_t :c ptr 'index)
                :line   (fslot-value-typed 'yaml_mark_t :c ptr 'line)
                :column (fslot-value-typed 'yaml_mark_t :c ptr 'column)))

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
  (let ((problem      (fslot-value parser 'problem))
        (problem_mark (fslot-value parser 'problem_mark)))
    (error 'yaml-parser-error :message (native-to-string problem)
                              :mark (make-yaml-mark problem_mark))))

(define-condition yaml-emitter-error (yaml-error)
  ((message :reader yaml-emitter-error-message
            :initarg :message
            :type simple-string
            :documentation "The error message (see `problem' field of `yaml_emitter_t')."))
  (:report
   (lambda (condition stream)
     (format stream "YAML emitter error: ~A.~&"
             (yaml-emitter-error-message condition)))))

(defun signal-yaml-emitter-error (emitter)
  (let ((problem (fslot-value emitter 'problem)))
    (error 'yaml-emitter-error :message (native-to-string problem))))

;;; Internal implementation
(defmacro with-libyaml-parser ((parser-var) &body body)
  `(with-stack-fobjects ((,parser-var 'yaml_parser_t))
     (when (= 0 (yaml_parser_initialize ,parser-var))
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

(def-foreign-type yaml_emitter_output_buffer
    ;; a utility struct for collecting emitter's output
    (:struct
     (buf (* :char))
     (size size_t)
     (pos  size_t)))

(defun-foreign-callable yaml-write-handler ((output (* yaml_emitter_output_buffer))
                                            (src    (* :char))
                                            (count  size_t))
  (declare (:returning :int)
           (type fixnum output)
           (optimize (speed 3) (safety 1)))
  (labels ((resize-maybe ()
             ;; it checks if the count exceeds the current size of the output buffer
             ;; if yes, it will grow the buffer (by factor 2)
             (let ((buf  (fslot-value-typed 'yaml_emitter_output_buffer :aligned output 'buf))
                   (pos  (fslot-value-typed 'yaml_emitter_output_buffer :aligned output 'pos))
                   (size (fslot-value-typed 'yaml_emitter_output_buffer :aligned output 'size))
                   (resize-p nil)
                   (newbuf nil))
               (when (>= (+ pos count) size)
                 (setq resize-p t)
                 (while (>= (+ pos count) size)
                   (setq size (* size 2))))
               (if* resize-p
                  then (setq newbuf (allocate-fobject :char :c size))
                       (memcpy newbuf buf pos)
                       (free-fobject buf)
                       (setf (fslot-value-typed 'yaml_emitter_output_buffer :aligned output 'buf)  newbuf
                             (fslot-value-typed 'yaml_emitter_output_buffer :aligned output 'size) size)
                       newbuf
                  else buf)))
           (write-to-buffer ()
             (let ((dest (resize-maybe))
                   (pos  (fslot-value-typed 'yaml_emitter_output_buffer :aligned output 'pos)))
               (memcpy (+ dest (* pos #.(sizeof-fobject :char))) src count)
               (setf (fslot-value-typed 'yaml_emitter_output_buffer :aligned output 'pos)
                     (+ pos count)))))
    ;; return 1 on success and 0 otherwise
    (handler-case (progn (write-to-buffer) 1)
      (error () 0))))

(defmacro with-libyaml-emitter ((emitter-var output-var) &body body)
  `(with-stack-fobjects ((,emitter-var 'yaml_emitter_t))
     ;; initialize emitter
     (when (= 0 (yaml_emitter_initialize ,emitter-var))
       (error 'libyaml-error :from 'yaml_emitter_initialize))
     (yaml_emitter_set_encoding ,emitter-var #.(position 'YAML_UTF8_ENCODING *enum-yaml-encoding*))
     (with-static-fobjects ((,output-var 'yaml_emitter_output_buffer :allocation :aligned))
       ;; initialize output (buffer)
       (setf (fslot-value-typed 'yaml_emitter_output_buffer :aligned ,output-var 'buf)  (allocate-fobject :char :c 16)
             (fslot-value-typed 'yaml_emitter_output_buffer :aligned ,output-var 'size) 16
             (fslot-value-typed 'yaml_emitter_output_buffer :aligned ,output-var 'pos)  0)
       (yaml_emitter_set_output emitter #.(register-foreign-callable 'yaml-write-handler) ,output-var)
       (unwind-protect
            (let ((*default-external-format* :utf-8))
              ,@body)
         ;; clean up
         (free-fobject (fslot-value-typed 'yaml_emitter_output_buffer :aligned ,output-var 'buf))
         (yaml_emitter_delete ,emitter-var)))))

(defun write-yaml-document (object)
  (let ((document (allocate-fobject 'yaml_document_t :c)))
    (when (= 0 (yaml_document_initialize document 0 0 0 0 1))
      (error 'libyaml-error :from 'yaml_document_initialize))
    (write-yaml-node object document)
    document))

(defun write-yaml-node (object document)
  (typecase object
    (yaml-sequence (write-yaml-sequence-node object document))
    (yaml-mapping (write-yaml-mapping-node object document))
    (t (write-yaml-scalar-node object document))))

(defun princ-yaml-scalar (object)
  (cond ((yaml-null-p object) "null")
        ((eql object 't) "true")
        ((eql object 'nil) "false")
        ((nanp object) "NaN")
        ((infinityp object) (if (minusp object) "-Inf" "Inf"))
        (t (typecase object
             (string object)
             (symbol (symbol-name object))
             (integer (princ-to-string object))
             (single-float (let ((*read-default-float-format* 'single-float))
                             (princ-to-string object)))
             (double-float (let ((*read-default-float-format* 'double-float))
                             (princ-to-string object)))
             (real (princ-to-string (float object)))))))

(defun write-yaml-scalar-node (object document)
  (with-native-string (value (princ-yaml-scalar object) :native-length-var len)
    (let ((scalar (yaml_document_add_scalar document 0 value len #.(position 'YAML_PLAIN_SCALAR_STYLE *enum-yaml-scalar-style*))))
      (when (= 0 scalar)
        (error 'libyaml-error :from 'yaml_document_add_scalar))
      scalar)))

(defun write-yaml-sequence-node (object document)
  (declare (type yaml-sequence object))
  (let ((sequence (yaml_document_add_sequence document 0 #.(position 'YAML_ANY_SEQUENCE_STYLE *enum-yaml-sequence-style*))))
    (when (= 0 sequence)
      (error 'libyaml-error :from 'yaml_document_add_sequence))
    (do ((i 0 (1+ i)))
        ((= i (length object)))
      (when (= 0 (yaml_document_append_sequence_item
                  document
                  sequence
                  (write-yaml-node (svref object i) document)))
        (error 'libyaml-error :from 'yaml_document_append_sequence_item)))
    sequence))

(defun write-yaml-mapping-node (object document)
  (declare (type yaml-mapping object))
  (let ((mapping (yaml_document_add_mapping document 0 #.(position 'YAML_ANY_MAPPING_STYLE  *enum-yaml-mapping-style*))))
    (when (= 0 mapping)
      (error 'libyaml-error :from 'yaml_document_add_mapping))
    (with-hash-table-iterator (gen object)
      (while t
        (multiple-value-bind (more? k-object v-object) (gen)
          (when (not more?) (return))
          (when (= 0 (yaml_document_append_mapping_pair
                      document
                      mapping
                      (write-yaml-node k-object document)
                      (write-yaml-node v-object document)))
            (error 'libyaml-error :from 'yaml_document_append_mapping_pair)))))
    mapping))

(defun yaml-emitter-open (emitter)
  (when (= 0 (yaml_emitter_open emitter))
    (signal-yaml-emitter-error emitter)))

(defun yaml-emitter-close (emitter)
  (when (= 0 (yaml_emitter_close emitter))
    (error 'libyaml-error :from 'yaml_emitter_close)))

(defun yaml-emitter-dump (emitter document)
  (when (= 0 (yaml_emitter_dump emitter document))
    (signal-yaml-emitter-error emitter)))

;;; Top-level APIs
(defun read-yaml (input &key pathname-p multiple)
  (if* pathname-p
     then (.read-yaml-file.   input multiple)
     else (.read-yaml-string. input multiple)))

(defun write-yaml (object)
  (with-libyaml-emitter (emitter output)
    (yaml-emitter-open emitter)
    (if* (consp object)
       then (dolist (ob object)
              (yaml-emitter-dump emitter (write-yaml-document ob)))
       else (yaml-emitter-dump emitter (write-yaml-document object)))
    (yaml-emitter-close emitter)
    (multiple-value-bind (str chars octets)
        (native-to-string (fslot-value-typed 'yaml_emitter_output_buffer :aligned output 'buf)
                          :length (fslot-value-typed 'yaml_emitter_output_buffer :aligned output 'pos))
      (declare (ignore chars octets))
      str)))

;;; load libyaml shared library
(eval-when (:load-toplevel)
  (let ((workdir (directory-namestring *load-pathname*)))
    (load #.(string+ #-windows "lib"
                     "yaml"
                     #\.
                     (car *load-foreign-types*))
          :foreign t
          :search-list (list workdir    ; the distributed libyaml shared library
                             (string+ workdir "../") ; for development environment
                             ))))
