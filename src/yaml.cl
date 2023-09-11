;;;; yaml.cl
(in-package #:yaml)

;;; Conditions
(define-condition yaml-error ()
  ()
  (:documentation "The base class of all YAML conditions."))

(define-condition libfyaml-error (yaml-error)
  ((from :reader libyaml-error-from
         :initarg :from
         :type symbol
         :documentation "The C function from libyaml that causes the error."))
  (:report
   (lambda (condition stream)
     (format stream "Error encountered whiling calling '~a"
             (libyaml-error-from condition)))))

(define-condition yaml-parse-error (yaml-error)
  ((message :reader yaml-error-message
            :initarg :message
            :type simple-string
            :documentation "The error message (see `problem' field of `yaml_parser_t')."))
  (:report
   (lambda (condition stream)
     (write-sequence (yaml-error-message condition) stream))))

;;; Internal implementation
(defun make-diag-output-buffer ()
  (let ((data (allocate-fobject 'fy_diag_output_buffer :aligned))
        (size 1))
    (declare (type fixnum data))
    (setf (fslot-value-typed 'fy_diag_output_buffer :aligned data 'buf)  (allocate-fobject :char :c size)
          (fslot-value-typed 'fy_diag_output_buffer :aligned data 'size) size
          (fslot-value-typed 'fy_diag_output_buffer :aligned data 'pos)  0)
    data))

(defun resize-diag-output-buffer-maybe (data new-bytes-count)
  (declare (type fixnum data new-bytes-count))
  (let ((buf  (fslot-value-typed 'fy_diag_output_buffer :aligned data 'buf))
        (pos  (fslot-value-typed 'fy_diag_output_buffer :aligned data 'pos))
        (size (fslot-value-typed 'fy_diag_output_buffer :aligned data 'size))
        (resize-p nil)
        (new-buf nil))
    (when (>= (+ pos new-bytes-count) size)
      (setq resize-p t)
      (while (>= (+ pos new-bytes-count) size)
        (setq size (* size 2))))
    (if* resize-p
       then (setq new-buf (allocate-fobject :char :c size))
            (memcpy new-buf buf pos)
            (free-fobject buf)
            (setf (fslot-value-typed 'fy_diag_output_buffer :aligned data 'buf)  new-buf
                  (fslot-value-typed 'fy_diag_output_buffer :aligned data 'size) size)
            new-buf
       else buf)))

(defun-foreign-callable fy_diag_output_error_fn ((diag :foreign-address)
                                                 (data (:aligned fy_diag_output_buffer))
                                                 (buf  (* :char))
                                                 (len  size_t))
  (declare (:returning :void)
           (ignore diag))
  (let ((user-buf (fslot-value-typed 'fy_diag_output_buffer :aligned data 'buf))
        (pos      (fslot-value-typed 'fy_diag_output_buffer :aligned data 'pos)))
    ;; resize if necessary
    (setq user-buf (resize-diag-output-buffer-maybe data len))
    ;; copy bytes and update pos
    (memcpy (+ user-buf (* pos #.(sizeof-fobject :char))) buf len)
    (setf (fslot-value-typed 'fy_diag_output_buffer :aligned data 'pos)
          (+ pos len))))

(defun signal-yaml-parse-error (diag-cfg)
  (let* ((user (fslot-value diag-cfg 'user))
         (buf (fslot-value-typed 'fy_diag_output_buffer :aligned user 'buf))
         (pos (fslot-value-typed 'fy_diag_output_buffer :aligned user 'pos)))
    (error 'yaml-parse-error :message (if (= pos 0) "unknown" (native-to-string buf :length pos)))))

(defun initialize-diag-cfg (cfg)
  (fy_diag_cfg_default cfg)
  (setf (fslot-value cfg 'fp)        0
        (fslot-value cfg 'output_fn) #.(register-foreign-callable 'fy_diag_output_error_fn :reuse nil)
        (fslot-value cfg 'user)      (make-diag-output-buffer)
        (fslot-value cfg 'level)     #.(position 'fyet_error *enum-fy-error-type*)
        (fslot-value cfg 'colorize)  0))

(defun initialize-parse-cfg (cfg diag)
  (setf (fslot-value cfg 'search_path) #.(string-to-native "")
        (fslot-value cfg 'flags)       #.(logior +FYPCF_COLLECT_DIAG+ +FYPCF_RESOLVE_DOCUMENT+)
        (fslot-value cfg 'userdata)    0
        (fslot-value cfg 'diag)        diag))

(defmacro with-parsing-environment ((&key parse-cfg diag-cfg diag) &body body)
  (let ((diag-cfg  (if diag-cfg diag-cfg (gensym "diag-cfg-")))
        (diag      (if diag diag (gensym "diag-")))
        (parse-cfg (if parse-cfg parse-cfg (gensym "parse-cfg-"))))
    `(with-stack-fobjects ((,diag-cfg  'fy_diag_cfg)
                           (,parse-cfg 'fy_parse_cfg))
       (initialize-diag-cfg ,diag-cfg)
       (let ((,diag (fy_diag_create ,diag-cfg)))
         (initialize-parse-cfg ,parse-cfg ,diag)
         (unwind-protect (progn ,@body)
           (free-fobject (fslot-value-typed 'fy_diag_output_buffer :aligned
                                            (fslot-value ,diag-cfg 'user)
                                            'buf))
           (fy_diag_destroy ,diag))))))

(defun load-yaml-document (document)
  (let ((root (fy_document_root document)))
    (if* (= root 0)
       then 'YAML_EMPTY_DOCUMENT
       else (unwind-protect (load-yaml-node root)
              (fy_document_destroy document)))))

(defun load-yaml-node (node)
  (case (fy_node_get_type node)
    (#.(position 'FYNT_SCALAR *enum-fy-node-type*)
     (load-yaml-scalar-node node))
    (#.(position 'FYNT_SEQUENCE *enum-fy-node-type*)
     (load-yaml-sequence-node node))
    (#.(position 'FYNT_MAPPING *enum-fy-node-type*)
     (load-yaml-mapping-node node))))

(defun yaml-node-tag (node)
  (with-stack-fobjects ((len* 'size_t))
    (let ((tag (fy_node_get_tag node len*)))
      (if* (= 0 tag)
         then nil
         else (native-to-string tag :length (fslot-value-typed 'size_t :foreign len*))))))

(defun yaml-node-style (node)
  (svref *enum-fy-node-style*
         (fy_node_get_style node)))

(defun yaml-node-scalar (node)
  (with-stack-fobjects ((len* 'size_t))
    (let* ((scalar (fy_node_get_scalar node len*)))
      (native-to-string scalar :length (fslot-value-typed 'size_t :foreign len*)))))

(defun load-yaml-scalar-node (node)
  (convert-yaml-scalar (yaml-node-scalar node)
                       (yaml-node-tag node)
                       (yaml-node-style node)))

(defun load-yaml-sequence-node (node)
  (loop with tag = (yaml-node-tag node)
        with style = (yaml-node-style node)
        with count = (fy_node_sequence_item_count node)
        with sv = (make-array count :element-type t)
        for i below count
        do (setf (svref sv i) (load-yaml-node (fy_node_sequence_get_by_index node i)))
        finally (return (convert-yaml-sequence sv tag style))))

(defun load-yaml-mapping-node (node)
  (loop with tag = (yaml-node-tag node)
        with style = (yaml-node-style node)
        with count = (fy_node_mapping_item_count node)
        with mapping = (make-hash-table :test 'equal :size (+ count 1) :rehash-threshold 1)
        for i below count
        for pair = (fy_node_mapping_get_by_index node i)
        for k = (fy_node_pair_key pair)
        for v = (fy_node_pair_value pair)
        do (setf (gethash (load-yaml-node k) mapping)
                 (load-yaml-node v))
        finally (return (convert-yaml-mapping mapping tag style))))

;;; Top-level APIs
(defgeneric read-yaml (input)
  (:documentation "Read a YAML document from `input'."))

(defmethod read-yaml ((str string))
  (with-parsing-environment (:parse-cfg parse-cfg :diag-cfg diag-cfg)
    (with-native-string (input str :native-length-var len)
      (let ((document (fy_document_build_from_string parse-cfg input len)))
        (when (= 0 document)
          (signal-yaml-parse-error diag-cfg))
        (load-yaml-document document)))))

(defmethod read-yaml ((path pathname))
  (if* (and (get-entry-point "fopen") (get-entry-point "fclose"))
     then (let ((fp (fopen (namestring path) "r")))
            (if* (zerop fp)
               then (read-yaml (file-contents path))
               else (unwind-protect
                         (with-parsing-environment (:parse-cfg parse-cfg :diag-cfg diag-cfg)
                           (let ((document (fy_document_build_from_fp parse-cfg fp)))
                             (when (= 0 document)
                               (signal-yaml-parse-error diag-cfg))
                             (load-yaml-document document)))
                      ;; close fp
                      (fclose fp))))
     else (read-yaml (file-contents path))))

(defgeneric read-yaml* (input)
  (:documentation "Read YAML document(s) into a list from `input'"))

(defmethod read-yaml* ((str string))
  (with-parsing-environment (:parse-cfg parse-cfg :diag-cfg diag-cfg)
    (with-native-string (input str :native-length-var len)
      (let ((parser (fy_parser_create parse-cfg)))
        (when (= 0 parser)
          (error 'libfyaml-error :from 'fy_parser_create))
        (when (/= 0 (fy_parser_set_string parser input len))
          (error 'libfyaml-error :from 'fy_parser_set_string))
        (let (doc garbage documents root)
          (unwind-protect
               (while t
                 (setq doc (fy_parse_load_document parser))
                 (when (= 0 doc) (return))
                 (setq root (fy_document_root doc))
                 (push doc garbage)
                 (push (load-yaml-node root) documents))
            (dolist (d garbage)
              (fy_parse_document_destroy parser d))
            (fy_parser_destroy parser))
          (nreverse documents))))))

;; Load libfyaml shared library
(eval-when (:load-toplevel)
  (let ((workdir (directory-namestring *load-pathname*)))
    (load #.(string+ "libfyaml" #\. (car *load-foreign-types*))
          :foreign t
          :search-list (list workdir   ; the distributed libfyaml shared library
                             (string+ workdir "../") ; for development environment
                             ))))
