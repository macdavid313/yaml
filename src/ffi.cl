;;;; ffi.cl
(in-package #:yaml)

(eval-when (:compile-toplevel)
  (declaim (optimize (speed 3) (safety 0) (space 0))))

;;; Utilities from libc
(def-foreign-type size_t :unsigned-nat)

(def-foreign-call fopen ((pathname (* :char) simple-string)
                         (mode (* :char) simple-string))
  :returning :foreign-address
  :strings-convert t
  :arg-checking nil
  :documentation " Opens the file whose name is specified in the parameter filename and associates
 it with a stream that can be identified in future operations by the FILE
 pointer returned.")

(def-foreign-call fclose ((fp :foreign-address))
  :returning :int
  :call-direct t
  :arg-checking nil
  :documentation "Closes the file associated with the `fp' and disassociates it.")

(def-foreign-call (.strtod. "strtod") ((str :foreign-address)
                                       (endptr :foreign-address))
  :returning :double
  :strings-convert nil
  :call-direct t
  :arg-checking nil)

(defun strtod (str)
  (declare (type simple-string str)
           (optimize (speed 3) (safety 0) (space 0)))
  (let ((*read-default-float-format* 'double-float))
    (the double-float (read-from-string str))))

(define-compiler-macro strtod (str &whole forms)
  (if* (get-entry-point "strtod")
     then (let ((s (gensym "s")))
            `(with-native-string (,s ,str)
               (.strtod. ,s 0)))
     else forms))

(def-foreign-call (.memcpy. "memcpy") ((dest (* :void))
                                       (src (* :void))
                                       (count size_t))
  :returning ((* :void))
  :arg-checking nil
  :call-direct t)

(defun memcpy (dest-ptr src-ptr count)
  (declare (type #+32bit (unsigned-byte 32) #+64bit (unsigned-byte 64)
                 dest-ptr
                 src-ptr))
  (do ((i 0 (1+ i)))
      ((= i count) nil)
    (setf (fslot-value-typed :char :c (+ dest-ptr (* i #.(sizeof-fobject :char))))
          (fslot-value-typed :char :c (+ src-ptr  (* i #.(sizeof-fobject :char)))))))

(define-compiler-macro memcpy (dest-ptr src-ptr count &whole form)
  (if* (get-entry-point "memcpy")
     then `(.memcpy. ,dest-ptr ,src-ptr ,count)
     else form))

;;; Enums
(def-foreign-type fy_error_type :int)
(defvar-nonbindable *enum-fy-error-type*
    #(FYET_DEBUG ; Debug level (disabled if library is not compiled in debug mode)
      FYET_INFO  ; Informational level
      FYET_NOTICE            ; Notice level
      FYET_WARNING           ; Warning level
      FYET_ERROR             ; Error level - error reporting is using this level
      FYET_MAX)              ; Non inclusive maximum fy_error_type value
  "The supported diagnostic/error types")

(def-foreign-type fy_error_module :int)
(defvar-nonbindable *enum-fy-error-module*
    #(FYEM_UNKNOWN           ; Unknown, default if not specific
      FYEM_ATOM              ; Atom module, used by atom chunking
      FYEM_SCAN              ; Scanner module
      FYEM_PARSE             ; Parser module
      FYEM_DOC               ; Document module
      FYEM_BUILD             ; Build document module (after tree is constructed)
      FYEM_INTERNAL          ; Internal error/diagnostic module
      FYEM_SYSTEM            ; System error/diagnostic module
      FYEM_MAX)              ; Non inclusive maximum fy_error_module value
  "Module which generated the diagnostic/error")

(def-foreign-type fy_parse_cfg_flags :int)

(defconstant +FYPCF_QUIET+               (ash 1 0)
  "Quiet, do not output any information messages")
(defconstant +FYPCF_COLLECT_DIAG+        (ash 1 1)
  "Collect diagnostic/error messages")
(defconstant +FYPCF_RESOLVE_DOCUMENT+    (ash 1 2)
  "When producing documents, automatically resolve them")
(defconstant +FYPCF_DISABLE_MMAP_OPT+     (ash 1 3)
  "Disable mmap optimization")
(defconstant +FYPCF_DISABLE_RECYCLING+    (ash 1 4)
  "Disable recycling optimization")
(defconstant +FYPCF_PARSE_COMMENTS+       (ash 1 5)
  "Enable parsing of comments (experimental)")
(defconstant +FYPCF_DISABLE_DEPTH_LIMIT+  (ash 1 6)
  "Disable depth limit check, use with enlarged stack")
(defconstant +FYPCF_DISABLE_ACCELERATORS+ (ash 1 7)
  "Disable use of access accelerators (saves memory)")
(defconstant +FYPCF_DISABLE_BUFFERING+    (ash 1 8)
  "Disable use of buffering where possible")

(defconstant .FYPCF_DEFAULT_VERSION_SHIFT. 9)
(defconstant .FYPCF_DEFAULT_VERSION_MASK.  (- (ash 1 5) 1))
(defconstant +FYPCF_DEFAULT_VERSION_AUTO+  (ash (logior 0 .FYPCF_DEFAULT_VERSION_MASK.) .FYPCF_DEFAULT_VERSION_SHIFT.)
  "Automatically use the most recent version the library supports")
(defconstant +FYPCF_DEFAULT_VERSION_1_1+   (ash (logior 1 .FYPCF_DEFAULT_VERSION_MASK.) .FYPCF_DEFAULT_VERSION_SHIFT.)
  "Default version is YAML 1.1")
(defconstant +FYPCF_DEFAULT_VERSION_1_2+   (ash (logior 2 .FYPCF_DEFAULT_VERSION_MASK.) .FYPCF_DEFAULT_VERSION_SHIFT.)
  "Default version is YAML 1.2")
(defconstant +FYPCF_DEFAULT_VERSION_1_3+   (ash (logior 3 .FYPCF_DEFAULT_VERSION_MASK.) .FYPCF_DEFAULT_VERSION_SHIFT.)
  "Default version is YAML 1.3 (experimental)")

(defconstant +FYPCF_SLOPPY_FLOW_INDENTATION+ (ash 1 14)
  "Allow sloppy indentation in flow mode")
(defconstant +FYPCF_PREFER_RECURSIVE+ (ash 1 15)
  "Prefer recursive algorighms instead of iterative whenever possible")

(defconstant .FYPCF_JSON_SHIFT. 16)
(defconstant .FYPCF_JSON_MASK.  (- (ash 1 2) 1))
(defconstant +FYPCF_JSON_AUTO+  (ash (logior 0 .FYPCF_JSON_MASK.) .FYPCF_JSON_SHIFT.)
  "Automatically enable JSON mode (when extension is .json)")
(defconstant +FYPCF_JSON_NONE+  (ash (logior 1 .FYPCF_JSON_MASK.) .FYPCF_JSON_SHIFT.)
  "Never enable JSON input mode")
(defconstant +FYPCF_JSON_FORCE+ (ash (logior 2 .FYPCF_JSON_MASK.) .FYPCF_JSON_SHIFT.)
  "Force JSON mode always")

(defconstant +FYPCF_YPATH_ALIASES+ (ash 1 18))
(defconstant +FYPCF_ALLOW_DUPLICATE_KEYS+ (ash 1 19))

(def-foreign-type fy_scalar_style :int)
(defconstant +FYSS_ANY+ -1 "Any scalar style, not generated by the parser. Lets the emitter to choose")
(defvar-nonbindable *enum-fy-scalar-style*
    #(FYSS_PLAIN                        ; Plain scalar style
      FYSS_SINGLE_QUOTED                ; Single quoted style
      FYSS_DOUBLE_QUOTED                ; Double quoted style
      FYSS_LITERAL                      ; YAML literal block style
      FYSS_FOLDED                       ; YAML folded block style
      FYSS_MAX)                         ; marks end of scalar styles
  "Scalar styles supported by the parser/emitter")

(def-foreign-type fy_node_type :int)
(defvar-nonbindable *enum-fy-node-type*
    #(FYNT_SCALAR                       ; Node is a scalar
      FYNT_SEQUENCE                     ; Node is a sequence
      FYNT_MAPPING)                     ; Node is a mapping
  "Each node may be one of the following types")

(def-foreign-type fy_node_style :int)
(defconstant +FYNS_ANY+ -1 "No hint, let the emitter decide")
(defvar-nonbindable *enum-fy-node-style*
    #(FYNS_FLOW               ; Prefer flow style (for sequence/mappings)
      FYNS_BLOCK              ; Prefer block style (for sequence/mappings)
      FYNS_PLAIN              ; Plain style preferred
      FYNS_SINGLE_QUOTED      ; Single quoted style preferred
      FYNS_DOUBLE_QUOTED      ; Double quoted style preferred
      FYNS_LITERAL            ; Literal style preferred (valid in block context)
      FYNS_FOLDED             ; Folded style preferred (valid in block context)
      FYNS_ALIAS)             ; It’s an alias
  "A node may contain a hint of how it should be rendered, encoded as a style.")

;;; Misc structs
;; The YAML version.
(def-foreign-type fy_version
    ;; The parser fills it according to the %YAML directive found in the document.
    (:struct
     ;; Major version number
     (major :int)
     ;; Major version number
     (minor :int)))

;; The YAML tag structure.
(def-foreign-type fy_tag
    ;; The parser fills it according to the %TAG directives encountered during parsing.
    (:struct
     ;; Handle of the tag (i.e. “!!” )
     (handle (* :char))
     ;; The prefix of the tag (i.e. “tag:yaml.org,2002:”
     (prefix (* :char))))

;; marker holding information about a location in a `struct fy_input'
(def-foreign-type fy_mark
    (:struct
     ;; Position of the mark (from the start of the current input)
     (input_pos size_t)
     ;; Line position (0 index based)
     (line :int)
     ;; Column position (0 index based)
     (column :int)))

;; a utility struct for collecting diag messages
(def-foreign-type fy_diag_output_buffer
    (:struct
     (buf (* :char))
     (size size_t)
     (pos size_t)))

;; fy_diag_cfg - The diagnostics configuration
(def-foreign-type fy_diag_cfg
    ;; This structure contains the configuration of the diagnostic object.
    (:struct
     (fp (* :void))                     ; File descriptor of the error output
     (output_fn (* :void))              ; Callback to use when fp is NULL
                                        ; typedef void (*fy_diag_output_fn)(struct fy_diag *diag, void *user, const char *buf, size_t len)
     (user (:aligned fy_diag_output_buffer)) ; User pointer to pass to the output_fn
     (level fy_error_type)                   ; The minimum debugging level
     (module_mask :unsigned-int)             ; A bitmask of the enabled modules
     (colorize :char)  ; true if output should be colorized using ANSI sequences
     (show_source :char)           ; true if source location should be displayed
     (show_position :char)         ; true if position should be displayed
     (show_type :char)             ; true if the type should be displayed
     (show_module :char)           ; true if the module should be displayed
     (source_width :int)           ; Width of the source field
     (position_width :int)         ; Width of the position field
     (type_width :int)             ; Width of the type field
     (module_width :int))) ; Width of the module field

;;; fy_parse_cfg - parser configuration structure.
(def-foreign-type fy_parse_cfg
    ;; Argument to the `fy_parser_create()' method which perform parsing of YAML files.
    (:struct
     ;; Search path when accessing files, seperate with ‘:’
     (search_path (* :char))
     ;; Configuration flags
     (flags fy_parse_cfg_flags)
     ;; Opaque user data pointer
     (userdata (* :void))
     ;; Optional diagnostic interface to use
     (diag (* :struct))))

;;; Misc APIs
(def-foreign-call fy_library_version (:void)
  :returning ((* :char) simple-string)
  :strings-convert t
  :arg-checking nil
  :documentation "A pointer to a version string of the form
<MAJOR>.<MINOR>[[.<PATCH>][-EXTRA-VERSION-INFO]]")

(def-foreign-call fy_error_type_to_string ((type fy_error_type))
  :returning ((* :char) simple-string)
  :strings-convert t
  :documentation "The string value of the error type or the empty string “” on error")

(def-foreign-call fy_error_module_to_string ((module fy_error_module))
  :returning ((* :char) simple-string)
  :strings-convert t
  :documentation "The string value of the error module or the empty string “” on error")

;;; Diagnostic
(def-foreign-call fy_diag_create ((cfg (* fy_diag_cfg)))
  :returning :foreign-address
  :arg-checking nil
  :call-direct t
  :documentation "Creates a diagnostic object using the provided configuration.")

(def-foreign-call fy_diag_destroy ((diag :foreign-address))
  :returning :void
  :arg-checking nil
  :call-direct t
  :documentation "Destroy a diagnostic objec. Note that the actual destruction only occurs when no
more references to the object are present. However no output will be generated
after this call.")

(def-foreign-call fy_diag_set_level ((diag :foreign-address)
                                     (level fy_error_type))
  :returning :void
  :arg-checking nil
  :call-direct t
  :documentation "Set a diagnostic object’s debug error level")

(def-foreign-call fy_diag_set_colorize ((diag :foreign-address)
                                        (colorize :char))
  :returning :void
  :arg-checking nil
  :call-direct t
  :documentation "Set a diagnostic object’s colorize option")

(def-foreign-call fy_diag_got_error ((diag :foreign-address))
  :returning :char
  :arg-checking nil
  :call-direct nil
  :documentation "Tests whether an error diagnostic has been produced.")

(def-foreign-call fy_diag_cfg_default ((cfg :foreign-address))
  :returning :void
  :arg-checking nil
  :call-direct t
  :documentation "Fills the configuration structure with defaults. The default always associates
the file descriptor to stderr.")

;;; Parser
(def-foreign-call fy_parser_create ((cfg (* fy_parse_cfg)))
  :returning :foreign-address
  :arg-checking nil
  :call-direct t
  :documentation "Creates a parser with its configuration cfg The parser may be destroyed by a
corresponding call to fy_parser_destroy().")

(def-foreign-call fy_parser_destroy ((parser :foreign-address))
  :returning :void
  :arg-checking nil
  :call-direct t
  :documentation "Destroy a parser created earlier via fy_parser_create().")

(def-foreign-call fy_parser_set_malloc_string ((parser :foreign-address)
                                               (str (* :char))
                                               (len size_t))
  :returning :int
  :strings-convert nil
  :arg-checking nil
  :call-direct t
  :documentation "Point the parser to the given (possible NULL terminated) string. Note that the
string is expected to be allocated via malloc(3) and ownership is transferred to
the created input. When the input is free’ed the memory will be automatically
freed.

In case of an error the string is not freed.")

(def-foreign-call fy_parse_load_document ((parser :foreign-address))
  :returning :foreign-address
  :strings-convert nil
  :arg-checking nil
  :call-direct t
  :documentation "This method performs parsing on a parser stream and returns the next document.
This means that for a compound document with multiple documents, each call will
return the next document.")

(def-foreign-call fy_parse_document_destroy ((parser :foreign-address)
                                             (document :foreign-address))
  :returning :void
  :strings-convert nil
  :arg-checking nil
  :call-direct t
  :documentation "Destroy a document created by fy_parse_load_document()")

(def-foreign-call fy_parser_get_diag ((parser :foreign-address))
  :returning :foreign-address
  :strings-convert nil
  :arg-checking nil
  :call-direct t
  :documentation "Return a pointer to the diagnostic object of a parser object. Note that the
returned diag object has a reference taken so you should `fy_diag_unref()' it
when you’re done with it.")

(def-foreign-call fy_parser_get_document_state ((parser :foreign-address))
  :returning :foreign-address
  :strings-convert nil
  :arg-checking nil
  :call-direct t
  :documentation "Retrieve the document state object of a parser. Note that this is only valid
during parsing.")

;;; Document State
(def-foreign-call fy_document_state_version ((state :foreign-address))
  :returning ((* fy_version))
  :strings-convert nil
  :arg-checking nil
  :call-direct t
  :documentation "Retrieve the version stored in a document state object")

(def-foreign-call fy_document_state_start_mark ((state :foreign-address))
  :returning ((* fy_mark))
  :strings-convert nil
  :arg-checking nil
  :call-direct t
  :documentation "Return the document state’s start mark (if it exists). Note that purely
synthetic documents do not contain one")

(def-foreign-call fy_document_state_end_mark ((state :foreign-address))
  :returning ((* fy_mark))
  :strings-convert nil
  :arg-checking nil
  :call-direct t
  :documentation "Return the document state’s end mark (if it exists). Note that purely synthetic
documents do not contain one")

(def-foreign-call fy_document_state_version_explicit ((state :foreign-address))
  :returning (:char boolean)
  :strings-convert nil
  :arg-checking nil
  :call-direct t
  :documentation "Find out if a document state object’s version was explicitly set in the
document. Note that for synthetic documents this method returns false.")

(def-foreign-call fy_document_state_tags_explicit ((state :foreign-address))
  :returning (:char boolean)
  :strings-convert nil
  :arg-checking nil
  :call-direct t
  :documentation "Find out if a document state object’s tags were explicitly set in the document.
Note that for synthetic documents this method returns false.")

(def-foreign-call fy_document_state_start_implicit ((state :foreign-address))
  :returning (:char boolean)
  :strings-convert nil
  :arg-checking nil
  :call-direct t
  :documentation "Find out if a document state object’s document was started implicitly. Note that
for synthetic documents this method returns false.")

(def-foreign-call fy_document_state_end_implicit ((state :foreign-address))
  :returning (:char boolean)
  :strings-convert nil
  :arg-checking nil
  :call-direct t
  :documentation "Find out if a document state object’s document was ended implicitly. Note that
for synthetic documents this method returns false.")

(def-foreign-call fy_document_state_tag_directive_iterate ((state :foreign-address)
                                                           (prevp (* (* :void))))
  :returning ((* fy_tag))
  :strings-convert nil
  :arg-checking nil
  :call-direct t
  :documentation "This method iterates over all the tag directives nodes in the document state
object. The start of the iteration is signalled by a NULL in *prevp.")

;;; Document
(def-foreign-call fy_document_create ((cfg (* fy_parse_cfg)))
  :returning :foreign-address
  :strings-convert nil
  :arg-checking nil
  :call-direct t
  :documentation "Create an empty document using the provided parser configuration. If NULL use
the default parse configuration.")

(def-foreign-call fy_document_build_from_string ((cfg (* fy_parse_cfg))
                                                 (str :foreign-address)
                                                 (len size_t))
  :returning :foreign-address
  :strings-convert nil
  :arg-checking nil
  :call-direct t
  :documentation "Create a document parsing the provided string as a YAML source.")

(def-foreign-call fy_document_build_from_fp ((cfg (* fy_parse_cfg))
                                             (fp :foreign-address))
  :returning :foreign-address
  :strings-convert nil
  :arg-checking nil
  :call-direct t
  :documentation "Create a document parsing the provided file pointer as a YAML source.")

(def-foreign-call fy_document_destroy ((document :foreign-address))
  :returning :void
  :strings-convert nil
  :arg-checking nil
  :call-direct t
  :documentation "Destroy a document (along with all children documents)")

(def-foreign-call fy_document_root ((document :foreign-address))
  :returning :foreign-address
  :strings-convert nil
  :arg-checking nil
  :call-direct t
  :documentation "Returns the root of the document. If the document is empty NULL will be returned
instead.")

(def-foreign-call fy_document_get_document_state ((document :foreign-address))
  :returning :foreign-address
  :strings-convert nil
  :arg-checking nil
  :call-direct t
  :documentation "Retrieve the document state object of a document.")

;;; Node
(def-foreign-call fy_node_free ((node :foreign-address))
  :returning :int
  :strings-convert nil
  :arg-checking nil
  :call-direct t
  :documentation "Recursively frees the given node releasing the memory it uses, removing any
anchors on the document it contains, and releasing references on the tokens it
contains.

This method will return an error if the node is attached, or if not NULL it is
not a member of a document.")

(def-foreign-call fy_node_get_type ((node :foreign-address))
  :returning fy_node_type
  :strings-convert nil
  :arg-checking nil
  :call-direct t
  :documentation "Retrieve the node type. It is one of FYNT_SCALAR, FYNT_SEQUENCE or FYNT_MAPPING.
A NULL node argument is a FYNT_SCALAR.")

(def-foreign-call fy_node_get_tag ((node :foreign-address)
                                   (len (* size_t)))
  :returning :foreign-address
  :strings-convert nil
  :arg-checking nil
  :call-direct t
  :documentation "This method will return a pointer to the text of a tag along with the length of
it. Note that this text is not NULL terminated.")

(def-foreign-call fy_node_get_style ((node :foreign-address))
  :returning fy_node_style
  :strings-convert nil
  :arg-checking nil
  :call-direct t
  :documentation "Retrieve the node rendering style. If the node is NULL then the style is
FYNS_PLAIN.")

(def-foreign-call fy_node_get_scalar ((node :foreign-address)
                                      (len (* size_t)))
  :returning :foreign-address
  :strings-convert nil
  :arg-checking nil
  :call-direct t
  :documentation "This method will return a pointer to the text of the scalar content of a node
along with the length of it. Note that this pointer is not NULL terminated.")

(def-foreign-call fy_node_sequence_item_count ((node :foreign-address))
  :returning :int
  :strings-convert nil
  :arg-checking nil
  :call-direct t
  :documentation "Get the item count of the sequence.")

(def-foreign-call fy_node_sequence_get_by_index ((node :foreign-address)
                                                 (idx :int))
  :returning :foreign-address
  :strings-convert nil
  :arg-checking nil
  :call-direct t
  :documentation "Retrieve a node in the sequence using it’s index. If index is positive or zero
the count is from the start of the sequence, while if negative from the end.
I.e. -1 returns the last item in the sequence.")

(def-foreign-call fy_node_mapping_item_count ((node :foreign-address))
  :returning :int
  :strings-convert nil
  :arg-checking nil
  :call-direct t
  :documentation "Get the count of the node pairs in the mapping.")

(def-foreign-call fy_node_mapping_get_by_index ((node :foreign-address)
                                                (idx :int))
  :returning :foreign-address
  :strings-convert nil
  :arg-checking nil
  :call-direct t
  :documentation "Retrieve a node pair in the mapping using its index. If index is positive or
zero the count is from the start of the sequence, while if negative from the
end. I.e. -1 returns the last node pair in the mapping.")

(def-foreign-call fy_node_pair_key ((node_pair :foreign-address))
  :returning :foreign-address
  :strings-convert nil
  :arg-checking nil
  :call-direct t
  :documentation "This method will return the node pair’s key. Note that this may be NULL, which
is returned also in case the node pair argument is NULL, so you should protect
against such a case.")

(def-foreign-call fy_node_pair_value ((node_pair :foreign-address))
  :returning :foreign-address
  :strings-convert nil
  :arg-checking nil
  :call-direct t
  :documentation "This method will return the node pair’s value. Note that this may be NULL, which
is returned also in case the node pair argument is NULL, so you should protect
against such a case.")
