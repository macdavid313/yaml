;;;; yaml.cl
(in-package #:cl-user)

(defpackage #:yaml
  (:use #:cl
        #:excl
        #:ff)
  (:export #:parse
           #:emit
           #:emit-to-string
           #:emit-pretty-as-document
           #:emit-pretty-as-document-to-string
           #:register-scalar-converter
           #:register-sequence-converter
           #:register-mapping-converter
           #:with-emitter-to-stream
           #:with-emitter-to-string
           #:emit-object
           #:print-scalar))

(in-package #:yaml)

;;; libyaml
(def-foreign-call yaml_get_version_string (:void)
  :documentation "Get the library version as a string."
  :returning ((* :char) simple-string)
  :strings-convert t)

(def-foreign-call yaml_get_version ((major (* :int))
                                    (minor (* :int))
                                    (patch (* :int)))
  :documentation "Get the library version numbers."
  :returning :void)

(def-foreign-type yaml_char_t :unsigned-char)

(def-foreign-type yaml_version_directive_t
    (:struct
     (major :int)
     (minor :int)))

(def-foreign-type yaml_tag_directive_t
    (:struct
     (handle (* yaml_char_t))
     (prefix (* yaml_char_t))))

(def-foreign-type yaml_encoding_t :int)

(defvar-nonbindable *enum-yaml-encoding*
    #(;; Let the parser choose the encoding.
      YAML_ANY_ENCODING
      ;; The default UTF-8 encoding.
      YAML_UTF8_ENCODING
      ;; The UTF-16-LE encoding with BOM.
      YAML_UTF16LE_ENCODING
      ;; The UTF-16-BE encoding with BOM.
      YAML_UTF16BE_ENCODING)
  "The stream encoding.")

(def-foreign-type yaml_break_t :int)

(defvar-nonbindable *enum-yaml-break*
    #(;; Let the parser choose the break type.
      YAML_ANY_BREAK
      ;; Use CR for line breaks (Mac style).
      YAML_CR_BREAK
      ;; Use LN for line breaks (Unix style).
      YAML_LN_BREAK
      ;; Use CR LN for line breaks (DOS style).
      YAML_CRLN_BREAK)
  "Line break types.")

(def-foreign-type yaml_error_type_t :int)

(defvar-nonbindable *enum-yaml-error-type*
    #(;; No error is produced.
      YAML_NO_ERROR

      ;; Cannot allocate or reallocate a block of memory.
      YAML_MEMORY_ERROR

      ;; Cannot read or decode the input stream.
      YAML_READER_ERROR
      ;; Cannot scan the input stream.
      YAML_SCANNER_ERROR
      ;; Cannot parse the input stream.
      YAML_PARSER_ERROR
      ;; Cannot compose a YAML document.
      YAML_COMPOSER_ERROR

      ;; Cannot write to the output stream.
      YAML_WRITER_ERROR
      ;; Cannot emit a YAML stream.
      YAML_EMITTER_ERROR)
  "Many bad things could happen with the parser and emitter.")

(def-foreign-type size_t :unsigned-nat)

(def-foreign-type yaml_mark_t
    (:struct
     (index size_t)
     (line size_t)
     (column size_t)))

(def-foreign-type yaml_scalar_style_t :int)

(defvar-nonbindable *enum-yaml-scalar-style*
    #(;; Let the emitter choose the style.
      YAML_ANY_SCALAR_STYLE

      ;; The plain scalar style.
      YAML_PLAIN_SCALAR_STYLE

      ;; The single-quoted scalar style.
      YAML_SINGLE_QUOTED_SCALAR_STYLE
      ;; The double-quoted scalar style.
      YAML_DOUBLE_QUOTED_SCALAR_STYLE

      ;; The literal scalar style.
      YAML_LITERAL_SCALAR_STYLE
      ;; The folded scalar style.
      YAML_FOLDED_SCALAR_STYLE)
  "Scalar styles.")

(def-foreign-type yaml_sequence_style_t :int)

(defvar-nonbindable *enum-yaml-sequence-style*
    #(;; Let the emitter choose the style.
      YAML_ANY_SEQUENCE_STYLE

      ;; The block sequence style.
      YAML_BLOCK_SEQUENCE_STYLE
      ;; The flow sequence style.
      YAML_FLOW_SEQUENCE_STYLE)
  "Sequence styles.")

(def-foreign-type yaml_mapping_style_t :int)

(defvar-nonbindable *enum-yaml-mapping-style*
    #(;; Let the emitter choose the style.
      YAML_ANY_MAPPING_STYLE

      ;; The block mapping style.
      YAML_BLOCK_MAPPING_STYLE
      ;; The flow mapping style.
      YAML_FLOW_MAPPING_STYLE)
  "Mapping styles.")

(def-foreign-type yaml_token_type_t :int)

(defvar-nonbindable *enum-yaml-token-type*
    #(;; An empty token.
      YAML_NO_TOKEN

      ;; A STREAM-START token.
      YAML_STREAM_START_TOKEN
      ;; A STREAM-END token.
      YAML_STREAM_END_TOKEN

      ;; A VERSION-DIRECTIVE token.
      YAML_VERSION_DIRECTIVE_TOKEN
      ;; A TAG-DIRECTIVE token.
      YAML_TAG_DIRECTIVE_TOKEN
      ;; A DOCUMENT-START token.
      YAML_DOCUMENT_START_TOKEN
      ;; A DOCUMENT-END token.
      YAML_DOCUMENT_END_TOKEN

      ;; A BLOCK-SEQUENCE-START token.
      YAML_BLOCK_SEQUENCE_START_TOKEN
      ;; A BLOCK-MAPPING-START token.
      YAML_BLOCK_MAPPING_START_TOKEN
      ;; A BLOCK-END token.
      YAML_BLOCK_END_TOKEN

      ;; A FLOW-SEQUENCE-START token.
      YAML_FLOW_SEQUENCE_START_TOKEN
      ;; A FLOW-SEQUENCE-END token.
      YAML_FLOW_SEQUENCE_END_TOKEN
      ;; A FLOW-MAPPING-START token.
      YAML_FLOW_MAPPING_START_TOKEN
      ;; A FLOW-MAPPING-END token.
      YAML_FLOW_MAPPING_END_TOKEN

      ;; A BLOCK-ENTRY token.
      YAML_BLOCK_ENTRY_TOKEN
      ;; A FLOW-ENTRY token.
      YAML_FLOW_ENTRY_TOKEN
      ;; A KEY token.
      YAML_KEY_TOKEN
      ;; A VALUE token.
      YAML_VALUE_TOKEN

      ;; An ALIAS token.
      YAML_ALIAS_TOKEN
      ;; An ANCHOR token.
      YAML_ANCHOR_TOKEN
      ;; A TAG token.
      YAML_TAG_TOKEN
      ;; A SCALAR token.
      YAML_SCALAR_TOKEN)
  "Token types.")

(def-foreign-type yaml_token_t
    (:struct
     ;; The token type.
     (type yaml_token_type_t)
     ;; The token data.
     (data (:union
            ;; The stream start (for @c YAML_STREAM_START_TOKEN).
            (stream_start (:struct
                           (encoding yaml_encoding_t)))
            ;; The alias (for @c YAML_ALIAS_TOKEN).
            (alias (:struct
                    (value (* yaml_char_t))))
            ;; The anchor (for @c YAML_ANCHOR_TOKEN).
            (anchor (:struct
                     (value (* yaml_char_t))))
            ;; The tag (for @c YAML_TAG_TOKEN).
            (tag (:struct
                  (handle (* yaml_char_t))
                  (suffix (* yaml_char_t))))
            ;; The scalar value (for @c YAML_SCALAR_TOKEN).
            (scalar (:struct
                     (value (* yaml_char_t))
                     (length size_t)
                     (style yaml_scalar_style_t)))
            ;; The version directive (for @c YAML_VERSION_DIRECTIVE_TOKEN).
            (version_directive (:struct
                                (major :int)
                                (minor :int)))
            ;; The tag directive (for @c YAML_TAG_DIRECTIVE_TOKEN).
            (tag_directive (:struct
                            (handle (* yaml_char_t))
                            (prefix (* yaml_char_t))))))
     ;; The beginning of the token.
     (start_mark yaml_mark_t)
     ;; The end of the token.
     (end_mark yaml_mark_t)))

(def-foreign-call yaml_token_delete ((token (* yaml_token_t)))
  :documentation "Free any memory allocated for a token object."
  :returning :void
  :call-direct t
  :arg-checking nil)

(def-foreign-type yaml_event_type_t :int)

(defvar-nonbindable *enum-yaml-event-type*
    #(;; An empty event.
      YAML_NO_EVENT

      ;; A STREAM-START event.
      YAML_STREAM_START_EVENT
      ;; A STREAM-END event.
      YAML_STREAM_END_EVENT

      ;; A DOCUMENT-START event.
      YAML_DOCUMENT_START_EVENT
      ;; A DOCUMENT-END event.
      YAML_DOCUMENT_END_EVENT

      ;; An ALIAS event.
      YAML_ALIAS_EVENT
      ;; A SCALAR event.
      YAML_SCALAR_EVENT

      ;; A SEQUENCE-START event.
      YAML_SEQUENCE_START_EVENT
      ;; A SEQUENCE-END event.
      YAML_SEQUENCE_END_EVENT

      ;; A MAPPING-START event.
      YAML_MAPPING_START_EVENT
      ;; A MAPPING-END event.
      YAML_MAPPING_END_EVENT)
  "Event types.")

(def-foreign-type yaml_event_t
    (:struct
     ;; The event type.
     (type yaml_event_type_t)
     ;; The event data.
     (data (:union
            ;; The stream parameters (for @c YAML_STREAM_START_EVENT).
            (stream_start (:struct
                           (encoding yaml_encoding_t)))
            ;; The document parameters (for @c YAML_DOCUMENT_START_EVENT).
            (document_start (:struct
                             (version_directive (* yaml_version_directive_t))
                             (tag_directives (:struct
                                              (start (* yaml_tag_directive_t))
                                              (end (* yaml_tag_directive_t))))
                             (implicit :int)))
            ;; The document end parameters (for @c YAML_DOCUMENT_END_EVENT).
            (document_end (:struct
                           (implicit :int)))
            ;; The alias parameters (for @c YAML_ALIAS_EVENT).
            (alias (:struct
                    (anchor (* yaml_char_t))))
            ;; The scalar parameters (for @c YAML_SCALAR_EVENT).
            (scalar (:struct
                     (anchor (* yaml_char_t))
                     (tag (* yaml_char_t))
                     (value (* yaml_char_t))
                     (length size_t)
                     (plain_implicit :int)
                     (quoted_implicit :int)
                     (style yaml_scalar_style_t)))
            ;; The sequence parameters (for @c YAML_SEQUENCE_START_EVENT).
            (sequence_start (:struct
                             (anchor (* yaml_char_t))
                             (tag (* yaml_char_t))
                             (implicit :int)
                             (style yaml_sequence_style_t)))
            ;; The mapping parameters (for @c YAML_MAPPING_START_EVENT).
            (mapping_start (:struct
                            (anchor (* yaml_char_t))
                            (tag (* yaml_char_t))
                            (implicit :int)
                            (style yaml_sequence_style_t)))))
     ;; The beginning of the event.
     (start_mark yaml_mark_t)
     ;; The end of the event.
     (end_mark yaml_mark_t)))

(def-foreign-call yaml_stream_start_event_initialize ((event (* yaml_event_t)) (encoding yaml_encoding_t))
  :documentation "Create the STREAM-START event."
  :returning :int
  :call-direct t
  :arg-checking nil)

(def-foreign-call yaml_stream_end_event_initialize ((event (* yaml_event_t)))
  :documentation "Create the STREAM-END event."
  :returning :int
  :call-direct t
  :arg-checking nil)

(def-foreign-call yaml_document_start_event_initialize ((event (* yaml_event_t))
                                                        (version_directive (* yaml_version_directive_t))
                                                        (tag_directives_start (* yaml_tag_directive_t))
                                                        (tag_directives_end (* yaml_tag_directive_t)))
  :documentation "Create the DOCUMENT-START event."
  :returning :int
  :call-direct t
  :arg-checking nil)

(def-foreign-call yaml_document_end_event_initialize ((event (* yaml_event_t))
                                                      (implicit :int))
  :documentation "Create the DOCUMENT-END event."
  :returning :int
  :call-direct t
  :arg-checking nil)

(def-foreign-call yaml_alias_event_initialize ((event (* yaml_event_t))
                                               (anchor (* yaml_char_t)))
  :documentation "Create an ALIAS event."
  :returning :int
  :call-direct t
  :arg-checking nil)

(def-foreign-call yaml_scalar_event_initialize ((event (* yaml_event_t))
                                                (anchor (* yaml_char_t))
                                                (tag (* yaml_char_t))
                                                (value (* yaml_char_t))
                                                (length :int)
                                                (plain_implicit :int)
                                                (quoted_implicit :int)
                                                (style yaml_scalar_style_t))
  :documentation "Create a SCALAR event."
  :returning :int
  :call-direct t
  :arg-checking nil)

(def-foreign-call yaml_sequence_start_event_initialize ((event (* yaml_event_t))
                                                        (anchor (* yaml_char_t))
                                                        (tag (* yaml_char_t))
                                                        (implicit :int)
                                                        (style yaml_sequence_style_t))
  :documentation "Create a SEQUENCE-START event."
  :returning :int
  :call-direct t
  :arg-checking nil)

(def-foreign-call yaml_sequence_end_event_initialize ((event (* yaml_event_t)))
  :documentation "Create a SEQUENCE-END event."
  :returning :int
  :call-direct t
  :arg-checking nil)

(def-foreign-call yaml_mapping_start_event_initialize ((event (* yaml_event_t))
                                                       (anchor (* yaml_char_t))
                                                       (tag (* yaml_char_t))
                                                       (implicit :int)
                                                       (style yaml_sequence_style_t))
  :documentation "Create a MAPPING-START event."
  :returning :int
  :call-direct t
  :arg-checking nil)

(def-foreign-call yaml_mapping_end_event_initialize ((event (* yaml_event_t)))
  :documentation "Create a MAPPING-END event."
  :returning :int
  :call-direct t
  :arg-checking nil)

(def-foreign-call yaml_event_delete ((event (* yaml_event_t)))
  :documentation "Free any memory allocated for an event object."
  :returning :void
  :call-direct t
  :arg-checking nil)

(defconstant +YAML_NULL_TAG+       "tag:yaml.org,2002:null"
  "The tag @c !!null with the only possible value: @c null.")
(defconstant +YAML_BOOL_TAG+       "tag:yaml.org,2002:bool"
  "The tag @c !!bool with the values: @c true and @c false.")
(defconstant +YAML_STR_TAG+        "tag:yaml.org,2002:str"
  "The tag @c !!str for string values.")
(defconstant +YAML_INT_TAG+        "tag:yaml.org,2002:int"
  "The tag @c !!int for integer values.")
(defconstant +YAML_FLOAT_TAG+      "tag:yaml.org,2002:float"
  "The tag @c !!float for float values.")
(defconstant +YAML_TIMESTAMP_TAG+  "tag:yaml.org,2002:timestamp"
  "The tag @c !!timestamp for date and time values.")
(defconstant +YAML_SEQ_TAG+        "tag:yaml.org,2002:seq"
  "The tag @c !!seq is used to denote sequences.")
(defconstant +YAML_MAP_TAG+        "tag:yaml.org,2002:map"
  "The tag @c !!map is used to denote mapping.")

(defconstant +YAML_DEFAULT_SCALAR_TAG+     +YAML_STR_TAG+
  "The default scalar tag is @c !!str.")
(defconstant +YAML_DEFAULT_SEQUENCE_TAG+   +YAML_SEQ_TAG+
  "The default sequence tag is @c !!seq.")
(defconstant +YAML_DEFAULT_MAPPING_TAG+    +YAML_MAP_TAG+
  "The default mapping tag is @c !!map.")

(def-foreign-type yaml_node_type_t :int)

(defvar-nonbindable *enum-yaml-node-type*
    #(;; An empty node.
      YAML_NO_NODE

      ;; A scalar node.
      YAML_SCALAR_NODE
      ;; A sequence node.
      YAML_SEQUENCE_NODE
      ;; A mapping node.
      YAML_MAPPING_NODE)
  "Node types.")

(def-foreign-type yaml_node_item_t :int)

(def-foreign-type yaml_node_pair_t
    (:struct
     (key :int)
     (value :int)))

(def-foreign-type yaml_node_t
    (:struct
     ;; The node type.
     (type yaml_node_type_t)
     ;; The node tag.
     (tag (* yaml_char_t))
     ;; The node data.
     (data (:union
            ;; The scalar parameters (for @c YAML_SCALAR_NODE).
            (scalar (:struct
                     (value (* yaml_char_t))
                     (length size_t)
                     (style yaml_scalar_style_t)))
            ;; The sequence parameters (for @c YAML_SEQUENCE_NODE).
            (sequence (:struct
                       (items (:struct
                               (start (* yaml_node_item_t))
                               (end (* yaml_node_item_t))
                               (top (* yaml_node_item_t))))
                       (style yaml_sequence_style_t)))
            ;; The mapping parameters (for @c YAML_MAPPING_NODE).
            (mapping (:struct
                      (pairs (:struct
                              (start (* yaml_node_pair_t))
                              (end (* yaml_node_pair_t))
                              (top (* yaml_node_pair_t))))
                      (style yaml_mapping_style_t)))))
     ;; The beginning of the node.
     (start_mark yaml_mark_t)
     ;; The end of the node.
     (end_mark yaml_mark_t)))

(def-foreign-type yaml_document_t
    (:struct
     ;; The document nodes.
     (nodes (:struct
             (start (* yaml_node_t))
             (end (* yaml_node_t))
             (top (* yaml_node_t))))

     ;; The version directive.
     (version_directive (* yaml_version_directive_t))

     ;; The list of tag directives.
     (tag_directives (:struct
                      (start (* yaml_tag_directive_t))
                      (end (* yaml_tag_directive_t))))

     ;; Is the document start indicator implicit?
     (start_implicit :int)
     ;; Is the document end indicator implicit?
     (end_implicit :int)

     ;; The beginning of the document.
     (start_mark yaml_mark_t)
     ;; The end of the document.
     (end_mark yaml_mark_t)))

(def-foreign-call yaml_document_initialize ((document (* yaml_document_t))
                                            (version_directive (* yaml_version_directive_t))
                                            (tag_directives_start (* yaml_tag_directive_t))
                                            (tag_directives_end (* yaml_tag_directive_t))
                                            (start_implicit :int)
                                            (end_implicit :int))
  :documentation "Create a YAML document."
  :returning :int
  :call-direct t
  :arg-checking nil)

(def-foreign-call yaml_document_delete ((document (* yaml_document_t)))
  :documentation "Delete a YAML document and all its nodes."
  :returning :void
  :call-direct t
  :arg-checking nil)

(def-foreign-call yaml_document_get_node ((document (* yaml_document_t))
                                          (index :int))
  :documentation "Get a node of a YAML document."
  :returning ((* yaml_node_t))
  :call-direct t
  :arg-checking nil)

(def-foreign-call yaml_document_get_root_node ((document (* yaml_document_t)))
  :documentation "Get the root of a YAML document node."
  :returning ((* yaml_node_t))
  :call-direct t
  :arg-checking nil)

(def-foreign-call yaml_document_add_scalar ((document (* yaml_document_t))
                                            (tag (* yaml_char_t))
                                            (value (* yaml_char_t))
                                            (length :int)
                                            (style yaml_scalar_style_t))
  :documentation "Create a SCALAR node and attach it to the document."
  :returning :int
  :call-direct t
  :arg-checking nil)

(def-foreign-call yaml_document_add_sequence ((document (* yaml_document_t))
                                              (tag (* yaml_char_t))
                                              (style yaml_sequence_style_t))
  :documentation "Create a SEQUENCE node and attach it to the document."
  :returning :int
  :call-direct t
  :arg-checking nil)

(def-foreign-call yaml_document_add_mapping ((document (* yaml_document_t))
                                             (tag (* yaml_char_t))
                                             (style yaml_mapping_style_t))
  :documentation "Create a MAPPING node and attach it to the document."
  :returning :int
  :call-direct t
  :arg-checking nil)

(def-foreign-call yaml_document_append_sequence_item ((document (* yaml_document_t))
                                                      (sequence :int)
                                                      (item :int))
  :documentation "Add an item to a SEQUENCE node."
  :returning :int
  :call-direct t
  :arg-checking nil)

(def-foreign-call yaml_document_append_mapping_pair ((document (* yaml_document_t))
                                                     (mapping :int)
                                                     (key :int)
                                                     (value :int))
  :documentation "Add a pair of a key and a value to a MAPPING node."
  :returning :int
  :call-direct t
  :arg-checking nil)

(def-foreign-type yaml_simple_key_t
    (:struct
     ;; Is a simple key possible?
     (possible :int)
     ;; Is a simple key required?
     (required :int)
     ;; The number of the token.
     (token_number size_t)
     ;; The position mark.
     (makr yaml_mark_t)))

(def-foreign-type yaml_parser_state_t :int)

(defvar-nonbindable *enum-yaml-parser-state*
    #(;; Expect STREAM-START.
      YAML_PARSE_STREAM_START_STATE
      ;; Expect the beginning of an implicit document.
      YAML_PARSE_IMPLICIT_DOCUMENT_START_STATE
      ;; Expect DOCUMENT-START.
      YAML_PARSE_DOCUMENT_START_STATE
      ;; Expect the content of a document.
      YAML_PARSE_DOCUMENT_CONTENT_STATE
      ;; Expect DOCUMENT-END.
      YAML_PARSE_DOCUMENT_END_STATE

      ;; Expect a block node.
      YAML_PARSE_BLOCK_NODE_STATE
      ;; Expect a block node or indentless sequence.
      YAML_PARSE_BLOCK_NODE_OR_INDENTLESS_SEQUENCE_STATE
      ;; Expect a flow node.
      YAML_PARSE_FLOW_NODE_STATE
      ;; Expect the first entry of a block sequence.
      YAML_PARSE_BLOCK_SEQUENCE_FIRST_ENTRY_STATE
      ;; Expect an entry of a block sequence.
      YAML_PARSE_BLOCK_SEQUENCE_ENTRY_STATE

      ;; Expect an entry of an indentless sequence.
      YAML_PARSE_INDENTLESS_SEQUENCE_ENTRY_STATE
      ;; Expect the first key of a block mapping.
      YAML_PARSE_BLOCK_MAPPING_FIRST_KEY_STATE
      ;; Expect a block mapping key.
      YAML_PARSE_BLOCK_MAPPING_KEY_STATE
      ;; Expect a block mapping value.
      YAML_PARSE_BLOCK_MAPPING_VALUE_STATE
      ;; Expect the first entry of a flow sequence.
      YAML_PARSE_FLOW_SEQUENCE_FIRST_ENTRY_STATE

      ;; Expect an entry of a flow sequence.
      YAML_PARSE_FLOW_SEQUENCE_ENTRY_STATE
      ;; Expect a key of an ordered mapping.
      YAML_PARSE_FLOW_SEQUENCE_ENTRY_MAPPING_KEY_STATE
      ;; Expect a value of an ordered mapping.
      YAML_PARSE_FLOW_SEQUENCE_ENTRY_MAPPING_VALUE_STATE
      ;; Expect the and of an ordered mapping entry.
      YAML_PARSE_FLOW_SEQUENCE_ENTRY_MAPPING_END_STATE
      ;; Expect the first key of a flow mapping.
      YAML_PARSE_FLOW_MAPPING_FIRST_KEY_STATE
      ;; Expect a key of a flow mapping.

      YAML_PARSE_FLOW_MAPPING_KEY_STATE
      ;; Expect a value of a flow mapping.
      YAML_PARSE_FLOW_MAPPING_VALUE_STATE
      ;; Expect an empty value of a flow mapping.
      YAML_PARSE_FLOW_MAPPING_EMPTY_VALUE_STATE
      ;; Expect nothing.
      YAML_PARSE_END_STATE)
  "The states of the parser.")

(def-foreign-type yaml_alias_data_t
    (:struct
     ;; The anchor.
     (anchor (* yaml_char_t))
     ;; The node id
     (index :int)
     ;; The anchor mark.
     (mark yaml_mark_t)))

(def-foreign-type yaml_parser_t
    (:struct
     ;; Error type.
     (error yaml_error_type_t)
     ;; Error description.
     (problem (* :char))
     ;; The byte about which the problem occured.
     (problem_offset size_t)
     ;; The problematic value (@c -1 is none).
     (problem_value :int)
     ;; The problem position.
     (problem_mark yaml_mark_t)
     ;; The error context.
     (context (* :char))
     ;; The context position.
     (context_mark yaml_mark_t)

     ;; Read handler.
     (read_handler (* :void)) ; typedef int yaml_read_handler_t(void *data, unsigned char *buffer, size_t size, size_t *size_read);
     ;; A pointer for passing to the read handler.
     (read_handler_data (* :void))

     ;; Standard (string or file) input data.
     (input (:union
             ;; String input data.
             (string (:struct
                      (start (* :unsigned-char))
                      (end (* :unsigned-char))
                      (current (* :unsigned-char))))
             ;; File input data.
             (file (* :void))))

     ;; EOF flag
     (eof :int)

     ;; The working buffer.
     (buffer (:struct
              (start (* yaml_char_t))
              (end (* yaml_char_t))
              (pointer (* yaml_char_t))
              (last (* yaml_char_t))))

     ;; The number of unread characters in the buffer.
     (unread size_t)

     ;; The raw buffer.
     (raw_buffer (:struct
                  (start (* :unsigned-char))
                  (end (* :unsigned-char))
                  (pointer (* :unsigned-char))
                  (last (* :unsigned-char))))

     ;; The input encoding.
     (encoding yaml_encoding_t)

     ;; The offset of the current position (in bytes).
     (offset size_t)

     ;; The mark of the current position.
     (mark yaml_mark_t)

     ;; Have we started to scan the input stream?
     (stream_start_produced :int)

     ;; Have we reached the end of the input stream?
     (stream_end_produced :int)

     ;; The number of unclosed '[' and '{' indicators.
     (flow_level :int)

     ;; The tokens queue.
     (tokens (:struct
              (start (* yaml_token_t))
              (end (* yaml_token_t))
              (head (* yaml_token_t))
              (tail (* yaml_token_t))))

     ;; The number of tokens fetched from the queue.
     (tokens_parsed size_t)

     ;; Does the tokens queue contain a token ready for dequeueing.
     (token_available :int)

     ;; The indentation levels stack.
     (indents (:struct
               (start (* :int))
               (end (* :int))
               (top (* :int))))

     ;; The current indentation level.
     (indent :int)

     ;; May a simple key occur at the current position?
     (simple_key_allowed :int)

     ;; The stack of simple keys.
     (simple_keys (:struct
                   (start (* yaml_simple_key_t))
                   (end (* yaml_simple_key_t))
                   (top (* yaml_simple_key_t))))

     ;; The parser states stack.
     (states (:struct
              (start (* yaml_parser_state_t))
              (end (* yaml_parser_state_t))
              (top (* yaml_parser_state_t))))

     ;; The current parser state.
     (state yaml_parser_state_t)

     ;; The stack of marks.
     (marks (:struct
             (start (* yaml_mark_t))
             (end (* yaml_mark_t))
             (top (* yaml_mark_t))))

     ;; The list of TAG directives.
     (tag_directives (:struct
                      (start (* yaml_tag_directive_t))
                      (end (* yaml_tag_directive_t))
                      (top (* yaml_tag_directive_t))))

     ;; The alias data.
     (aliases (:struct
               (start (* yaml_alias_data_t))
               (end (* yaml_alias_data_t))
               (top (* yaml_alias_data_t))))

     ;; The currently parsed document.
     (document (* yaml_document_t))))

(def-foreign-call yaml_parser_initialize ((parser (* yaml_parser_t)))
  :documentation "Initialize a parser."
  :returning :int
  :call-direct t
  :arg-checking nil)

(def-foreign-call yaml_parser_delete ((parser (* yaml_parser_t)))
  :documentation "Destroy a parser."
  :returning :void
  :call-direct t
  :arg-checking nil)

(def-foreign-call yaml_parser_set_input_string ((parser (* yaml_parser_t))
                                                (input (* :unsigned-char))
                                                (size size_t))
  :documentation "Set a string input."
  :returning :void
  :call-direct t
  :arg-checking nil)

(def-foreign-call yaml_parser_set_input_file ((parser (* yaml_parser_t))
                                              (file (* :void)))
  :documentation "Set a file input."
  :returning :void
  :call-direct t
  :arg-checking nil)

(def-foreign-call yaml_parser_set_input ((parser (* yaml_parser_t))
                                         (handler (* :void))
                                         (data (* :void)))
  :documentation "Set a generic input handler."
  :returning :void
  :call-direct t
  :arg-checking nil)

(def-foreign-call yaml_parser_set_encoding ((parser (* yaml_parser_t))
                                            (encoding yaml_encoding_t))
  :documentation "Set the source encoding."
  :returning :void
  :call-direct t
  :arg-checking nil)

(def-foreign-call yaml_parser_scan ((parser (* yaml_parser_t))
                                    (token (* yaml_token_t)))
  :documentation "Scan the input stream and produce the next token."
  :returning :int
  :call-direct t
  :arg-checking nil)

(def-foreign-call yaml_parser_parse ((parser (* yaml_parser_t))
                                     (event (* yaml_event_t)))
  :documentation "Parse the input stream and produce the next parsing event."
  :returning :int
  :call-direct t
  :arg-checking nil)

(def-foreign-call yaml_parser_load ((parser (* yaml_parser_t))
                                    (document (* yaml_document_t)))
  :documentation "Parse the input stream and produce the next YAML document."
  :returning :int
  :call-direct t
  :arg-checking nil)

(def-foreign-type yaml_emitter_state_t :int)

(defvar-nonbindable *enum-yaml-emitter-state*
    #(;; Expect STREAM-START.
      YAML_EMIT_STREAM_START_STATE
      ;; Expect the first DOCUMENT-START or STREAM-END.
      YAML_EMIT_FIRST_DOCUMENT_START_STATE
      ;; Expect DOCUMENT-START or STREAM-END.
      YAML_EMIT_DOCUMENT_START_STATE
      ;; Expect the content of a document.
      YAML_EMIT_DOCUMENT_CONTENT_STATE
      ;; Expect DOCUMENT-END.
      YAML_EMIT_DOCUMENT_END_STATE

      ;; Expect the first item of a flow sequence.
      YAML_EMIT_FLOW_SEQUENCE_FIRST_ITEM_STATE
      ;; Expect an item of a flow sequence.
      YAML_EMIT_FLOW_SEQUENCE_ITEM_STATE
      ;; Expect the first key of a flow mapping.
      YAML_EMIT_FLOW_MAPPING_FIRST_KEY_STATE
      ;; Expect a key of a flow mapping.
      YAML_EMIT_FLOW_MAPPING_KEY_STATE
      ;; Expect a value for a simple key of a flow mapping.
      YAML_EMIT_FLOW_MAPPING_SIMPLE_VALUE_STATE

      ;; Expect a value of a flow mapping.
      YAML_EMIT_FLOW_MAPPING_VALUE_STATE
      ;; Expect the first item of a block sequence.
      YAML_EMIT_BLOCK_SEQUENCE_FIRST_ITEM_STATE
      ;; Expect an item of a block sequence.
      YAML_EMIT_BLOCK_SEQUENCE_ITEM_STATE
      ;; Expect the first key of a block mapping.
      YAML_EMIT_BLOCK_MAPPING_FIRST_KEY_STATE
      ;; Expect the key of a block mapping.
      YAML_EMIT_BLOCK_MAPPING_KEY_STATE

      ;; Expect a value for a simple key of a block mapping.
      YAML_EMIT_BLOCK_MAPPING_SIMPLE_VALUE_STATE
      ;; Expect a value of a block mapping.
      YAML_EMIT_BLOCK_MAPPING_VALUE_STATE
      ;; Expect nothing.
      YAML_EMIT_END_STATE)
  "The emitter states.")

(def-foreign-type yaml_anchors_t
    (:struct
     ;; The number of references.
     (references :int)
     ;; The anchor id.
     (anchor :int)
     ;; If the node has been emitted?
     (serialized :int)))

(def-foreign-type yaml_emitter_t
    (:struct
     ;; Error type.
     (error yaml_error_type_t)
     ;; Error description.
     (problem (* :char))

     ;; Write handler.
     (write_handler (* :void)) ; typedef int yaml_write_handler_t(void *data, unsigned char *buffer, size_t size);

     ;; A pointer for passing to the write handler.
     (write_handler_data (* :void))

     ;; Standard (string or file) output data.
     (output (:union
              (:struct
               (buffer (* :unsigned-char))
               (size size_t)
               (size_written (* size_t)))
              (file (* :void))))

     ;; The working buffer.
     (buffer (:struct
              (start (* yaml_char_t))
              (end (* yaml_char_t))
              (pointer (* yaml_char_t))
              (last (* yaml_char_t))))

     ;; The raw buffer.
     (raw_buffer (:struct
                  (start (* yaml_char_t))
                  (end (* yaml_char_t))
                  (pointer (* yaml_char_t))
                  (last (* yaml_char_t))))

     ;; The stream encoding.
     (encoding yaml_encoding_t)

     ;; If the output is in the canonical style?
     (canonical :int)
     ;; The number of indentation spaces.
     (best_indent :int)
     ;; The preferred width of the output lines.
     (best_width :int)
     ;; Allow unescaped non-ASCII characters?
     (unicode :int)
     ;; The preferred line break.
     (line_break yaml_break_t)

     ;; The stack of states.
     (states (:struct
              (start (* yaml_emitter_state_t))
              (end (* yaml_emitter_state_t))
              (top (* yaml_emitter_state_t))))

     ;; The current emitter state.
     (state yaml_emitter_state_t)

     ;; The event queue.
     (events (:struct
              (start (* yaml_event_t))
              (end (* yaml_event_t))
              (head (* yaml_event_t))
              (tail (* yaml_event_t))))

     ;; The stack of indentation levels.
     (indents (:struct
               (start (* :int))
               (end (* :int))
               (top (* :int))))

     ;; The list of tag directives.
     (tag_directives (:struct
                      (start (* yaml_tag_directive_t))
                      (end (* yaml_tag_directive_t))
                      (top (* yaml_tag_directive_t))))

     ;; The current indentation level.
     (indent :int)

     ;; The current flow level.
     (flow_level :int)

     ;; Is it the document root context?
     (root_context :int)
     ;; Is it a sequence context?
     (sequence_context :int)
     ;; Is it a mapping context?
     (mapping_context :int)
     ;; Is it a simple mapping key context?
     (simple_key_context :int)

     ;; The current line.
     (line :int)
     ;; The current column.
     (column :int)
     ;; If the last character was a whitespace?
     (whitespace :int)
     ;; If the last character was an indentation character (' ', '-', '?', ':')?
     (indention :int)
     ;; If an explicit document end is required?
     (open_ended :int)

     ;; Anchor analysis.
     (anchor_data (:struct
                   (anchor (* yaml_char_t))
                   (anchor_length size_t)
                   (alias :int)))

     ;; Tag analysis.
     (tag_data (:struct
                (handle (* yaml_char_t))
                (handle_length size_t)
                (suffix (* yaml_char_t))
                (suffix_length size_t)))

     ;; Scalar analysis.
     (scalar_data (:struct
                   (value (* yaml_char_t))
                   (length size_t)
                   (multiline :int)
                   (flow_plain_allowed :int)
                   (block_plain_allowed :int)
                   (single_quoted_allowed :int)
                   (block_allowed :int)
                   (style yaml_scalar_style_t)))

     ;; If the stream was already opened?
     (opened :int)
     ;; If the stream was already closed?
     (closed :int)

     ;; The information associated with the document nodes.
     (anchors (* yaml_anchors_t))

     ;; The last assigned anchor id.
     (last_anchor_id :int)

     ;; The currently emitted document.
     (document (* yaml_document_t))))

(def-foreign-call yaml_emitter_initialize ((emitter (* yaml_emitter_t)))
  :documentation "Initialize an emitter."
  :returning :int
  :call-direct t
  :arg-checking nil)

(def-foreign-call yaml_emitter_delete ((emitter (* yaml_emitter_t)))
  :documentation "Destroy an emitter."
  :returning :void
  :call-direct t
  :arg-checking nil)

(def-foreign-call yaml_emitter_set_output_string ((emitter (* yaml_emitter_t))
                                                  (output (* :unsigned-char))
                                                  (size size_t)
                                                  (size_written (* size_t)))
  :documentation "Set a string output."
  :returning :void
  :call-direct t
  :arg-checking nil)

(def-foreign-call yaml_emitter_set_output_file ((emitter (* yaml_emitter_t))
                                                (file (* :void)))
  :documentation "Set a file output."
  :returning :void
  :call-direct t
  :arg-checking nil)

(def-foreign-call yaml_emitter_set_output ((emitter (* yaml_emitter_t))
                                           (handler (* :void))
                                           (data (* :void)))
  :documentation "Set a generic output handler."
  :returning :void
  :call-direct t
  :arg-checking nil)

(def-foreign-call yaml_emitter_set_encoding ((emitter (* yaml_emitter_t))
                                             (encoding yaml_encoding_t))
  :documentation "Set the output encoding."
  :returning :void
  :call-direct t
  :arg-checking nil)

(def-foreign-call yaml_emitter_set_canonical ((emitter (* yaml_emitter_t))
                                              (canonical :int))
  :documentation "Set if the output should be in the \"canonical\" format as in the YAML specification."
  :returning :void
  :call-direct t
  :arg-checking nil)

(def-foreign-call yaml_emitter_set_indent ((emitter (* yaml_emitter_t))
                                           (indent :int))
  :documentation "Set the indentation increment."
  :returning :void
  :call-direct t
  :arg-checking nil)

(def-foreign-call yaml_emitter_set_width ((emitter (* yaml_emitter_t))
                                          (width :int))
  :documentation "Set the preferred line width. @c -1 means unlimited."
  :returning :void
  :call-direct t
  :arg-checking nil)

(def-foreign-call yaml_emitter_set_unicode ((emitter (* yaml_emitter_t))
                                            (unicode :int))
  :documentation "Set if unescaped non-ASCII characters are allowed."
  :returning :void
  :call-direct t
  :arg-checking nil)

(def-foreign-call yaml_emitter_set_break ((emitter (* yaml_emitter_t))
                                          (line_break yaml_break_t))
  :documentation "Set the preferred line break."
  :returning :void
  :call-direct t
  :arg-checking nil)

(def-foreign-call yaml_emitter_emit ((emitter (* yaml_emitter_t))
                                     (event (* yaml_event_t)))
  :documentation "Emit an event.
The event object may be generated using the yaml_parser_parse() function.
The emitter takes the responsibility for the event object and destroys its
content after it is emitted. The event object is destroyed even if the
function fails."
  :returning :int
  :call-direct t
  :arg-checking nil)

(def-foreign-call yaml_emitter_open ((emitter (* yaml_emitter_t)))
  :documentation "Start a YAML stream.
This function should be used before yaml_emitter_dump() is called."
  :returning :int
  :call-direct t
  :arg-checking nil)

(def-foreign-call yaml_emitter_close ((emitter (* yaml_emitter_t)))
  :documentation "Finish a YAML stream.
This function should be used after yaml_emitter_dump() is called."
  :returning :int
  :call-direct t
  :arg-checking nil)

(def-foreign-call yaml_emitter_dump ((emitter (* yaml_emitter_t))
                                     (document (* yaml_document_t)))
  :documentation "Emit a YAML document.
The documen object may be generated using the yaml_parser_load() function
or the yaml_document_initialize() function.  The emitter takes the
responsibility for the document object and destroys its content after
it is emitted. The document object is destroyed even if the function fails."
  :returning :int
  :call-direct t
  :arg-checking nil)

(def-foreign-call yaml_emitter_flush ((emitter (* yaml_emitter_t)))
  :documentation "Flush the accumulated characters to the output."
  :returning :int
  :call-direct t
  :arg-checking nil)

;;; High-level APIs
(defun parse (string-or-pathname)
  (etypecase string-or-pathname
    (string (with-stack-fobjects ((parser 'yaml_parser_t)
                                  (event 'yaml_event_t))
              (yaml_parser_initialize parser)
              (with-native-string (input string-or-pathname :native-length-var size)
                (yaml_parser_set_input_string parser input size)
                (loop for flag = (when (zerop (yaml_parser_parse parser event))
                                   (error (native-to-string (fslot-value-typed 'yaml_parser_t :foreign parser 'problem))))
                      for event-type = (svref *enum-yaml-event-type* (fslot-value-typed 'yaml_event_t :foreign event 'type))
                      until (eql event-type 'YAML_STREAM_END_EVENT)
                      do (print event-type)
                      finally (progn (yaml_parser_delete parser)
                                     (yaml_event_delete event))))))
    (pathname )))
