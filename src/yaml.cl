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

(defmacro with-parse-cfg ((cfg-var &key diag-cfg-var) &body body)
  (let ((diag-cfg (if diag-cfg-var diag-cfg-var (gensym "diag-cfg")))
        (diag (gensym "diag")))
    `(with-stack-fobjects ((,diag-cfg 'fy_diag_cfg)
                           (,cfg-var  'fy_parse_cfg))
       (initialize-diag-cfg ,diag-cfg)
       (let ((,diag (fy_diag_create ,diag-cfg)))
         (setf (fslot-value ,cfg-var 'search_path) #.(string-to-native "")
               (fslot-value ,cfg-var 'flags)       #.(logior (position 'FYPCF_COLLECT_DIAG *enum-fy-parse-cfg-flags*)
                                                             (position 'FYPCF_RESOLVE_DOCUMENT *enum-fy-parse-cfg-flags*))
               (fslot-value ,cfg-var 'userdata)    0
               (fslot-value ,cfg-var 'diag)        ,diag)
         (unwind-protect (progn ,@body)
           (free-fobject (fslot-value-typed 'fy_diag_output_buffer :aligned
                                            (fslot-value ,diag-cfg 'user)
                                            'buf))
           (fy_diag_destroy ,diag))))))

(defmacro with-yaml-parser ((parser-var) &body body)
  (with-parse-cfg (cfg)
    `(let ((,parser-var (fy_parser_create ,cfg)))
       (unwind-protect (progn ,@body)
         (fy_parser_destroy ,parser-var)))))

;;; Top-level APIs
(defgeneric read-yaml (input)
  (:documentation "Read a YAML document from `input'."))

(defmethod read-yaml ((str string))
  (with-parse-cfg (cfg :diag-cfg-var diag-cfg)
    (let ((document (fy_document_build_from_string cfg
                                                   (string-to-native str :null-terminate t)
                                                   -1)))
      (when (= 0 document)
        (signal-yaml-parse-error diag-cfg)))))

;;; Load libfyaml shared library
(eval-when (:load-toplevel)
  (let ((workdir (directory-namestring *load-pathname*)))
    (load #.(string+ "libfyaml" #\. (car *load-foreign-types*))
          :foreign t
          :search-list (list workdir   ; the distributed libfyaml shared library
                             (string+ workdir "../") ; for development environment
                             ))))
