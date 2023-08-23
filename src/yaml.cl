;;;; yaml.cl
(in-package #:yaml)

(define-condition yaml-error ()
  ()
  (:documentation "The base class of all YAML conditions."))

(define-condition yaml-parser-error (yaml-error)
  ((message :reader message
            :initarg :message
            :type simple-string
            :documentation "The error message (see `problem' field of `yaml_parser_t').")
   (line :reader line
         :initarg :line
         :type integer
         :documentation "The line where the error happened.")
   (column :reader column
           :initarg :column
           :type integer
           :documentation "The column where the error happened."))
  (:report
   (lambda (condition stream)
     (format stream "YAML parser error at line ~A, column ~A: ~A.~&"
             (line condition)
             (column condition)
             (message condition)))))

(defun signal-yaml-parser-error (parser)
  (let ((problem (fslot-value-typed 'yaml_parser_t :foreign parser 'problem))
        (mark (fslot-value-typed 'yaml_parser_t :foreign parser 'mark)))
    (declare (type #+32bit (unsigned-byte 32) #+64bit (unsigned-byte 64) problem mark)
             (dynamic-extent problem mark))
    (error 'yaml-parser-error :message (native-to-string problem)
                              :line    (fslot-value-typed 'yaml_mark_t :c mark 'line)
                              :column  (fslot-value-typed 'yaml_mark_t :c mark 'column))))

(defun yaml-parse-string (s)
  (with-stack-fobjects ((parser 'yaml_parser_t)
                        (event 'yaml_event_t))
    (when (zerop (yaml_parser_initialize parser))
      (error "Error encountered while calling 'yaml_parser_initialize"))
    (with-native-string (input s :native-length-var size)
      (yaml_parser_set_input_string parser input size)
      (unwind-protect
           (loop (when (zerop (yaml_parser_parse parser event))
                   (signal-yaml-parser-error parser))
                 (ecase (fslot-value-typed 'yaml_event_t :foreign event 'type)
                   (#.(position 'YAML_NO_EVENT *enum-yaml-event-type*) nil)

                   (#.(position 'YAML_STREAM_START_EVENT *enum-yaml-event-type*) nil)
                   (#.(position 'YAML_STREAM_END_EVENT *enum-yaml-event-type*) (return t))

                   (#.(position 'YAML_DOCUMENT_START_EVENT *enum-yaml-event-type*) nil)
                   (#.(position 'YAML_DOCUMENT_END_EVENT *enum-yaml-event-type*) nil)

                   (#.(position 'YAML_ALIAS_EVENT *enum-yaml-event-type*) nil)
                   (#.(position 'YAML_SCALAR_EVENT *enum-yaml-event-type*) nil)

                   (#.(position 'YAML_SEQUENCE_START_EVENT *enum-yaml-event-type*) nil)
                   (#.(position 'YAML_SEQUENCE_END_EVENT *enum-yaml-event-type*) nil)

                   (#.(position 'YAML_MAPPING_START_EVENT *enum-yaml-event-type*) nil)
                   (#.(position 'YAML_MAPPING_END_EVENT *enum-yaml-event-type*) nil))))
      ;; cleanup forms
      (yaml_parser_delete parser)
      (yaml_event_delete event))))

(defun parse (string-or-pathname)
  (etypecase string-or-pathname
    (string (yaml-parse-string string-or-pathname))
    (pathname (yaml-parse-string
               (with-output-to-string (str)
                 (with-open-file (in string-or-pathname :direction :input)
                   (loop for line = (read-line in nil nil)
                         while line
                         do (progn (write-sequence line str)
                                   (terpri str)))))))))
