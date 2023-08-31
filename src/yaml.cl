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

(defmacro with-yaml-parser-and-event ((parser-var event-var) &body body)
  `(with-stack-fobjects ((,parser-var 'yaml_parser_t)
                         (,event-var  'yaml_event_t))
     (when (zerop (yaml_parser_initialize ,parser-var))
       (error "Error encountered while calling 'yaml_parser_initialize"))
     (unwind-protect (progn ,@body)
       (yaml_parser_delete ,parser-var)
       (yaml_event_delete ,event-var))))

;;; Conditions
(define-condition yaml-error ()
  ()
  (:documentation "The base class of all YAML conditions."))

(define-condition yaml-parser-error (yaml-error)
  ((message :reader yaml-parser-error-message
            :initarg :message
            :type simple-string
            :documentation "The error message (see `problem' field of `yaml_parser_t').")
   (line    :reader yaml-parser-error-line
            :initarg :line
            :type fixnum
            :documentation "The line where the error happened.")
   (column  :reader yaml-parser-error-column
            :initarg :column
            :type fixnum
            :documentation "The column where the error happened."))
  (:report
   (lambda (condition stream)
     (format stream "YAML parser error at line ~A, column ~A: ~A.~&"
             (yaml-parser-error-line    condition)
             (yaml-parser-error-column  condition)
             (yaml-parser-error-message condition)))))

(defun signal-yaml-parser-error (parser)
  (let ((problem (fslot-value-typed 'yaml_parser_t :foreign parser 'problem))
        (problem_mark (fslot-value-typed 'yaml_parser_t :foreign parser 'problem_mark)))
    (error 'yaml-parser-error :message (native-to-string problem)
                              :line    (fslot-value-typed 'yaml_mark_t :c problem_mark 'line)
                              :column  (fslot-value-typed 'yaml_mark_t :c problem_mark 'column))))

;;; Scalar
;; (defun parse-yaml-scalar (event parsed)
;;   (let ((value (native-to-string (fslot-value-typed 'yaml_event_t :foreign event 'data 'scalar 'value)
;;                                  :length (fslot-value-typed 'yaml_event_t :foreign event 'data 'scalar 'length)))
;;         (tag (fslot-value-typed 'yaml_event_t :foreign event 'data 'scalar 'tag))
;;         (style (svref *enum-yaml-scalar-style* (fslot-value-typed 'yaml_event_t :foreign event 'data 'scalar 'style))))
;;     (print (list value tag style))))

;;; Sequence

;;; Mapping

;;; Parser
(defun parse-yaml (parser event)
  (let (parsed
        parser-finished-p)
    (while (not parser-finished-p)
      (when (zerop (yaml_parser_parse parser event))
        (signal-yaml-parser-error parser))
      (case (fslot-value-typed 'yaml_event_t :foreign event 'type)

        ;; do nothing for 'YAML_STREAM_START_EVENT and 'YAML_NO_EVENT
        (#.(position 'YAML_STREAM_START_EVENT *enum-yaml-event-type*)
         #.(position 'YAML_NO_EVENT *enum-yaml-event-type*)
         nil)

        (#.(position 'YAML_STREAM_END_EVENT *enum-yaml-event-type*)
         (setq parser-finished-p t))

        (#.(position 'YAML_DOCUMENT_START_EVENT *enum-yaml-event-type*))

        (#.(position 'YAML_DOCUMENT_END_EVENT *enum-yaml-event-type*))

        (#.(position 'YAML_ALIAS_EVENT *enum-yaml-event-type*))

        (#.(position 'YAML_SCALAR_EVENT *enum-yaml-event-type*))

        (#.(position 'YAML_SEQUENCE_START_EVENT *enum-yaml-event-type*))
        (#.(position 'YAML_SEQUENCE_END_EVENT *enum-yaml-event-type*))

        (#.(position 'YAML_MAPPING_START_EVENT *enum-yaml-event-type*))
        (#.(position 'YAML_MAPPING_END_EVENT *enum-yaml-event-type*))))
    (nreverse parsed)))

;;; Top-level APIs
(defun parse (string-or-pathname)
  (with-yaml-parser-and-event (parser event)
    (etypecase string-or-pathname
      (string (let ((s string-or-pathname))
                (with-native-string (input s :native-length-var size)
                  (yaml_parser_set_input_string parser input size)
                  (parse-yaml parser event))))
      (pathname (let ((filespec string-or-pathname))
                  (assert (probe-file filespec))
                  (if* (and (get-entry-point "fopen") (get-entry-point "fclose"))
                     then (let ((fp (fopen (namestring filespec) "r")))
                            (if* (zerop fp)
                               then (parse (file-contents filespec :external-format :utf-8))
                               else (unwind-protect
                                         (progn (yaml_parser_set_input_file parser fp)
                                                (parse-yaml parser event))
                                      (assert (zerop (fclose fp))))))
                     else (parse (file-contents filespec :external-format :utf-8))))))))

(defun emit (data)
  data
  'not-implemented)
