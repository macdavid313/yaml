;;;; yaml.cl
(in-package #:yaml)

(def-foreign-call fopen ((pathname (* :char) simple-string)
                         (mode (* :char) simple-string))
  :returning :foreign-address
  :strings-convert t
  :arg-checking nil)

(def-foreign-call fclose ((fp :foreign-address))
  :returning :int
  :call-direct t
  :arg-checking nil)

(defmacro with-yaml-parser-context ((parser-var document-count-var) &body body)
  `(with-stack-fobjects ((,parser-var 'yaml_parser_t))
     (when (zerop (yaml_parser_initialize ,parser-var))
       (error 'libyaml-error :from 'yaml_parser_initialize))
     (unwind-protect (let ((,document-count-var 0)) ,@body)
       (yaml_parser_delete ,parser-var))))

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
             (yaml-parser-error-message condition)
             (yaml-mark-line (yaml-error-mark condition))
             (yaml-mark-column (yaml-error-mark condition))))))

(defun signal-yaml-parser-error (parser)
  (let ((problem (fslot-value-typed 'yaml_parser_t :foreign parser 'problem))
        (problem_mark (fslot-value-typed 'yaml_parser_t :foreign parser 'problem_mark)))
    (error 'yaml-parser-error :message (native-to-string problem)
                              :mark (make-yaml-mark problem_mark))))

(define-condition yaml-unknown-anchor-error (yaml-error)
  ((anchor :reader yaml-unknown-anchor-error-anchor
           :initarg :anchor
           :type simple-string
           :documentation "The unknown anchor name.")
   (mark :reader yaml-error-mark
         :initarg :mark
         :type yaml-mark
         :documentation "The start mark of the current event."))
  (:report
   (lambda (condition stream)
     (format stream "Unknown anchor ~s found at line ~a, column ~a.~&"
             (yaml-unknown-anchor-error-anchor condition)
             (yaml-mark-line (yaml-error-mark condition))
             (yaml-mark-column (yaml-error-mark condition))))))

(defstruct (yaml-mark (:constructor mk-yaml-mark))
  (index 0 :type fixnum)
  (line 0 :type fixnum)
  (column 0 :type fixnum))

(defun make-yaml-mark (mark)
  (mk-yaml-mark :index  (fslot-value-typed 'yaml_mark_t :c mark 'index)
                :line   (fslot-value-typed 'yaml_mark_t :c mark 'line)
                :column (fslot-value-typed 'yaml_mark_t :c mark 'column)))

(defstruct (yaml-event (:constructor mk-yaml-event))
  type
  data
  start-mark
  end-mark
  alias-id)

(defun make-yaml-event (event)
  (flet ((event-data-to-lisp-value (event-type)
           (case event-type
             ('YAML_STREAM_START_EVENT
              (svref *enum-yaml-encoding* (fslot-value-typed 'yaml_event_t :foreign event 'data 'stream_start 'encoding)))

             ('YAML_DOCUMENT_START_EVENT
              (let ((version-directive (fslot-value-typed 'yaml_event_t :foreign event 'data 'document_start 'version_directive))
                    (tag-directives nil) ; TODO: implement tag-directives?
                    (implicit-p (fslot-value-typed 'yaml_event_t :foreign event 'data 'sequence_start 'implicit)))
                (list (if (zerop version-directive) nil (native-to-string version-directive))
                      tag-directives
                      (not (zerop implicit-p)))))

             ('YAML_DOCUMENT_END_EVENT
              (not (zerop (fslot-value-typed 'yaml_event_t :foreign event 'data 'document_end 'implicit))))

             ('YAML_ALIAS_EVENT
              (native-to-string
               (fslot-value-typed 'yaml_event_t :foreign event 'data 'alias 'anchor)))

             ('YAML_SCALAR_EVENT
              (let ((anchor (fslot-value-typed 'yaml_event_t :foreign event 'data 'scalar 'anchor))
                    (tag (fslot-value-typed 'yaml_event_t :foreign event 'data 'scalar 'tag))
                    (value (fslot-value-typed 'yaml_event_t :foreign event 'data 'scalar 'value))
                    (length (fslot-value-typed 'yaml_event_t :foreign event 'data 'scalar 'length))
                    (plain-implicit-p (fslot-value-typed 'yaml_event_t :foreign event 'data 'scalar 'plain_implicit))
                    (quoted-implicit-p (fslot-value-typed 'yaml_event_t :foreign event 'data 'scalar 'quoted_implicit))
                    (style (fslot-value-typed 'yaml_event_t :foreign event 'data 'scalar 'style)))
                (list (if (zerop anchor) nil (native-to-string anchor))
                      (if (zerop tag) nil (native-to-string tag))
                      (if (zerop value) nil (native-to-string value :length length))
                      length
                      (not (zerop plain-implicit-p))
                      (not (zerop quoted-implicit-p))
                      (svref *enum-yaml-scalar-style* style))))

             ('YAML_SEQUENCE_START_EVENT
              (let ((anchor (fslot-value-typed 'yaml_event_t :foreign event 'data 'sequence_start 'anchor))
                    (tag (fslot-value-typed 'yaml_event_t :foreign event 'data 'sequence_start 'tag))
                    (implicit-p (fslot-value-typed 'yaml_event_t :foreign event 'data 'sequence_start 'implicit))
                    (style (fslot-value-typed 'yaml_event_t :foreign event 'data 'sequence_start 'style)))
                (list (if (zerop anchor) nil (native-to-string anchor))
                      (if (zerop tag) nil (native-to-string tag))
                      (not (zerop implicit-p))
                      (svref *enum-yaml-sequence-style* style))))

             ('YAML_MAPPING_START_EVENT
              (let ((anchor (fslot-value-typed 'yaml_event_t :foreign event 'data 'mapping_start 'anchor))
                    (tag (fslot-value-typed 'yaml_event_t :foreign event 'data 'mapping_start 'tag))
                    (implicit-p (fslot-value-typed 'yaml_event_t :foreign event 'data 'mapping_start 'implicit))
                    (style (fslot-value-typed 'yaml_event_t :foreign event 'data 'mapping_start 'style)))
                (list (if (zerop anchor) nil (native-to-string anchor))
                      (if (zerop tag) nil (native-to-string tag))
                      (not (zerop implicit-p))
                      (svref *enum-yaml-mapping-style* style)))))))
    (let ((event-type (svref *enum-yaml-event-type* (fslot-value-typed 'yaml_event_t :foreign event 'type))))
      (mk-yaml-event :type event-type
                     :data (event-data-to-lisp-value event-type)
                     :start-mark (make-yaml-mark (fslot-value-typed 'yaml_event_t :foreign event 'start_mark))
                     :end-mark (make-yaml-mark (fslot-value-typed 'yaml_event_t :foreign event 'end_mark))))))

(defun yaml-events-to-document (events aliases)
  (declare (type (simple-array yaml-event (*)) events)
           (type hash-table aliases)
           (ignorable aliases))
  (do ((idx 0)
       (event (aref events 0))
       document)
      ((= idx (length events)) document)
    (case (yaml-event-type event)
      (YAML_SCALAR_EVENT)
      (YAML_SEQUENCE_START_EVENT)
      (YAML_MAPPING_START_EVENT))
    (incf idx)))

(defun parse-next-document (parser document-count)
  (labels ((next-event ()
             (with-static-fobjects ((event 'yaml_event_t))
               (unwind-protect
                    (progn (when (zerop (yaml_parser_parse parser event))
                             (signal-yaml-parser-error parser))
                           (make-yaml-event event))
                 (yaml_event_delete event))))
           (scan-events ()
             (let (;; (first-document-p (zerop document-count))
                   events
                   (anchors (make-hash-table :test 'excl::simple-string=))
                   (aliases (make-hash-table :test 'excl::=_2op)))
               (declare (dynamic-extent anchors))
               (incf document-count)
               (while t
                 (let ((event (next-event)))
                   (case (yaml-event-type event)
                     ;; skipped events:
                     ;; YAML_NO_EVENT
                     ;; YAML_STREAM_START_EVENT
                     ;; YAML_DOCUMENT_START_EVENT

                     ;; return
                     (YAML_STREAM_END_EVENT
                      (return))

                     ;; return
                     (YAML_DOCUMENT_END_EVENT
                      (return))

                     (YAML_ALIAS_EVENT
                      (let* ((anchor (yaml-event-data event))
                             (id (gethash anchor anchors)))
                        (if* id
                           then (setf (yaml-event-alias-id event) id)
                                (push event events)
                           else (error 'yaml-unknown-anchor-error :anchor anchor
                                                                  :mark (yaml-event-start-mark event)))))

                     ((YAML_SCALAR_EVENT YAML_SEQUENCE_START_EVENT YAML_MAPPING_START_EVENT)
                      (let ((anchor (car (yaml-event-data event))))
                        (when anchor
                          (let ((id (hash-table-count anchors)))
                            (setf (gethash anchor anchors) id
                                  (gethash id aliases) (length events)))))
                      (push event events))

                     ((YAML_SEQUENCE_END_EVENT YAML_MAPPING_END_EVENT)
                      (push event events)))))
               (values (make-array (length events) :element-type 'yaml-event :initial-contents (nreverse events))
                       aliases))))
    (multiple-value-call 'yaml-events-to-document (scan-events))))

;;; Top-level APIs
(defun parse (string-or-pathname)
  (with-yaml-parser-context (parser document-count)
    (etypecase string-or-pathname
      (string (let ((s string-or-pathname))
                (with-native-string (input s :native-length-var size)
                  (yaml_parser_set_input_string parser input size)
                  (parse-next-document parser document-count))))
      (pathname (let ((filespec string-or-pathname))
                  (assert (probe-file filespec))
                  (if* (and (get-entry-point "fopen") (get-entry-point "fclose"))
                     then (let ((fp (fopen (namestring filespec) "r")))
                            (if* (zerop fp)
                               then (parse (file-contents filespec :external-format :utf-8))
                               else (unwind-protect
                                         (progn (yaml_parser_set_input_file parser fp)
                                                (parse-next-document parser document-count))
                                      (assert (zerop (fclose fp))))))
                     else (parse (file-contents filespec :external-format :utf-8))))))))

(defun emit (data)
  data
  'not-implemented)
