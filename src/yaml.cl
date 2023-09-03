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

(defun libyaml-event-type (event)
  (declare (type fixnum event))
  (svref *enum-yaml-event-type*
         (fslot-value-typed 'yaml_event_t :aligned event 'type)))

(defstruct (yaml-mark (:constructor mk-yaml-mark))
  (index 0 :type fixnum)
  (line 0 :type fixnum)
  (column 0 :type fixnum))

(defun libyaml-event-start-mark (ptr)
  (declare (type fixnum ptr))
  (make-yaml-mark (fslot-value-typed 'yaml_event_t :aligned ptr 'start_mark)))

(defun libyaml-event-end-mark (ptr)
  (declare (type fixnum ptr))
  (make-yaml-mark (fslot-value-typed 'yaml_event_t :aligned ptr 'end_mark)))

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

(define-condition yaml-unexpected-event-type-error (yaml-error)
  ((expect :initarg :expect
           :documentation "The expected event.")
   (actual :initarg :actual
           :documentation "The actual event.")
   (mark :reader yaml-error-mark
         :initarg :mark
         :type yaml-mark
         :documentation "The start mark of the current event."))
  (:report (lambda (condition stream)
             (with-slots (expect actual mark) condition
               (format stream "Expecting ~s type of event but got ~s at line ~a, column ~a.~&"
                       expect
                       actual
                       (yaml-mark-line (yaml-error-mark condition))
                       (yaml-mark-column (yaml-error-mark condition)))))))

(define-condition yaml-unexpected-end-of-events-error (yaml-error)
  ())

;;; Internal implementations
(defun parse-scalar (value style)
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
    ((match-re "^([-+]?[0-9]+)$" value)
     (parse-integer value :junk-allowed nil))
    ;; Octal digits
    ((match-re "^0o([0-7]+)$" value)
     (parse-integer value :start 2 :radix 8 :junk-allowed nil))
    ;; Hex digits
    ((match-re "^0x([0-9a-fA-F]+)$" value)
     (parse-integer value :start 2 :radix 16 :junk-allowed nil))
    ;; Floating-point number
    ((match-re "^[-+]?(\\.[0-9]+|[0-9]+(\\.[0-9]*)?)([eE][-+]?[0-9]+)?$" value)
     (if* (get-entry-point "strtod")
        then (with-native-string (str value)
               (strtod str 0))
        else (let ((*read-default-float-format* 'double-float))
               (read-from-string value))))
    ;; NaN
    ((member value '(".nan" ".NaN" ".NAN") :test 'equal)
     *nan-double*)
    ;; +Inf
    ((match-re "^[+]?(\\.inf|\\.Inf|\\.INF)$" value)
     *infinity-double*)
    ;; -Inf
    ((match-re "^-(\\.inf|\\.Inf|\\.INF)$" value)
     *negative-infinity-double*)
    ;; Just a string
    (t value)))

(defun make-yaml-document (events aliases)
  (declare (type (array fixnum (*)) events)
           (type hash-table aliases)
           (ignorable aliases))
  (let ((pos 0)
        document)
    (labels ((next ()
               (incf pos)
               (when (> pos (1- (length events)))
                 (error 'yaml-unexpected-end-of-events-error))
               (aref events pos))

             (peek ()
               (let ((next-pos (1+ pos)))
                 (when (< next-pos (length events))
                   (aref events next-pos))))

             (parse-event (event)
               (let ((event-type (libyaml-event-type event)))
                 (case event-type
                   (YAML_SCALAR_EVENT
                    (let ((anchor (fslot-value-typed 'yaml_event_t :aligned event 'data 'scalar 'anchor))
                          (value (fslot-value-typed 'yaml_event_t :aligned event 'data 'scalar 'value))
                          (length (fslot-value-typed 'yaml_event_t :aligned event 'data 'scalar 'length))
                          (style (fslot-value-typed 'yaml_event_t :aligned event 'data 'scalar 'style)))
                      (if* (= 0 anchor)
                         then (setq value (native-to-string value :length length))
                              (parse-scalar value (svref *enum-yaml-scalar-style* style))
                         else (setq anchor (native-to-string anchor :length length)) ; TODO: handle anchor
                              nil)))

                   (YAML_SEQUENCE_START_EVENT
                    (let ()
                      (do (sequence)
                          ((eq (libyaml-event-type (peek)) 'YAML_SEQUENCE_END_EVENT)
                           ;; return
                           (incf pos)
                           (make-array (length sequence) :element-type t :initial-contents (nreverse sequence)))
                        (declare (dynamic-extent sequence))
                        (if* (null (peek))
                           then (error 'yaml-unexpected-event-type-error :expect 'YAML_SEQUENCE_END_EVENT
                                                                         :actual :eof
                                                                         :mark (make-yaml-mark (libyaml-event-end-mark event)))
                           else (push (parse-event (next)) sequence)))))

                   (YAML_MAPPING_START_EVENT
                    (let ()
                      (do ((mapping (make-hash-table :test 'equal)))
                          ((eq (libyaml-event-type (peek)) 'YAML_MAPPING_END_EVENT)
                           ;; return
                           (incf pos)
                           mapping)
                        (cond ((null (peek))
                               (error 'yaml-unexpected-event-type-error :expect 'YAML_MAPPING_END_EVENT
                                                                        :actual :eof
                                                                        :mark (make-yaml-mark (libyaml-event-end-mark event))))
                              ((not (eq (libyaml-event-type (peek)) 'YAML_SCALAR_EVENT))
                               (error 'yaml-unexpected-event-type-error :expect 'YAML_SCALAR_EVENT
                                                                        :actual (libyaml-event-type (peek))
                                                                        :mark (make-yaml-mark (libyaml-event-start-mark (peek)))))
                              (t (setf (gethash (parse-event (next)) mapping)
                                       (parse-event (next))))))))))))
      (while (< pos (1- (length events)))
        (let ((event (aref events pos)))
          (push (parse-event event) document))))
    (nreverse document)))

(defun parse-next-document (parser)
  (let ((events (make-array 0 :element-type 'fixnum :adjustable t :fill-pointer 0))
        (anchors (make-hash-table :test #'equal))
        (aliases (make-hash-table :test #'eql))
        (event-alias-id (make-hash-table :test #'eql)))
    (declare (dynamic-extent events anchors))
    (unwind-protect
         (progn
           ;; start scanning
           (while t
             (let ((event (allocate-fobject 'yaml_event_t :aligned)))
               ;; get next event (from parser)
               (when (zerop (yaml_parser_parse parser (aligned-to-address event)))
                 (signal-yaml-parser-error parser))
               (let ((event-type (libyaml-event-type event)))
                 (case event-type

                   ;; skipped events:
                   ;; - yaml-event/no-event
                   ;; - yaml-event/stream-start
                   ;; - yaml-event/document-start

                   (YAML_STREAM_END_EVENT
                    (return))

                   (YAML_DOCUMENT_END_EVENT
                    (return))

                   (YAML_ALIAS_EVENT
                    (let* ((anchor (native-to-string
                                    (fslot-value-typed 'yaml_event_t :aligned event 'data 'alias 'anchor)))
                           (id (gethash anchor anchors)))
                      (if* id
                         then (setf (gethash event event-alias-id) id)
                              (vector-push-extend event events)
                         else (error 'yaml-unknown-anchor-error :anchor anchor
                                                                :mark (fslot-value-typed 'yaml_event_t :aligned event 'start_mark)))))

                   ((YAML_SCALAR_EVENT YAML_SEQUENCE_START_EVENT YAML_MAPPING_START_EVENT)
                    (let ((anchor (fslot-value-typed 'yaml_event_t :aligned event
                                                     'data
                                                     (case event-type
                                                       (YAML_SCALAR_EVENT 'scalar)
                                                       (YAML_SEQUENCE_START_EVENT 'sequence_start)
                                                       (YAML_MAPPING_START_EVENT 'mapping_start))
                                                     'anchor)))
                      (when (/= 0 anchor)
                        (setq anchor (native-to-string anchor))
                        (let ((id (hash-table-count anchors)))
                          (setf (gethash anchor anchors) id
                                (gethash id aliases) (length events)))))
                    (vector-push-extend event events))

                   ((YAML_SEQUENCE_END_EVENT YAML_MAPPING_END_EVENT)
                    (vector-push-extend event events))))))
           ;; end of scanning (while t ... )
           (make-yaml-document events aliases))
      ;; cleanup events
      (do ((i 0 (1+ i)))
          ((= i (length events)))
        (let ((event (aref events i)))
          (yaml_event_delete (aligned-to-address event))
          (free-fobject-aligned event))))))

;;; Top-level APIs
(defun parse-yaml-string (str)
  (with-libyaml-parser (parser)
    (with-native-string (input str :native-length-var size)
      (yaml_parser_set_input_string parser input size)
      (parse-next-document parser))))

(defun parse-yaml-file (filespec)
  (assert (probe-file filespec))
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
                                (parse-next-document ,parser))
                           (assert (zerop (fclose ,fp))))))))
     else forms))

;; (defun-foreign-callable yaml-read-handler ((data :lisp)
;;                                            (buffer (* :unsigned-char))
;;                                            (size size_t)
;;                                            (size_read (* size_t)))
;;   (declare (:lisp-args-will-not-move t)
;;            (:returning :int))
;;   (if* (ignore-errors (let ((i 0))
;;                         (while (< i size)
;;                           (let ((byte (read-byte data :nil :eof)))
;;                             (if* (eq byte :eof)
;;                                then (return)
;;                                else (setf (fslot-value-typed :unsigned-char :c (+ buffer (* i #.(sizeof-fobject :unsigned-char))))
;;                                           byte))))
;;                         (setf (fslot-value-typed 'size_t :c size_read) i)))
;;      then 1
;;      else 0))

(defun emit (data)
  data
  'not-implemented)
