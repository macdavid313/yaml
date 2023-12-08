;;;; parser.cl
(in-package #:yaml)

(define-condition yaml-parser-error (simple-error)
  ())

(define-condition yaml-invalid-escape-sequence-error (yaml-parser-error)
  ())

(define-condition yaml-malformed-error (yaml-parser-error)
  ())

(define-condition yaml-nested-documents-error (yaml-parser-error)
  ())

(define-condition yaml-unexpected-eof-error (yaml-parser-error)
  ())

(define-condition yaml-unexpected-token-error (yaml-parser-error)
  ())

(define-condition yaml-unhandled-error (yaml-parser-error)
  ())

(defstruct token-iterator
  buffer
  pos)

(defun next-token (iterator)
  (let ((token (peek-token iterator)))
    (if* token
       then (incf (token-iterator-pos iterator))
            token
       else nil)))

(defun peek-token (iterator)
  (with-slots (buffer pos) iterator
    (if* (>= pos (length buffer))
       then nil
       else (aref buffer pos))))

(defun reset-token-iterator (iterator)
  (setf (token-iterator-pos iterator) 0))

(defun seek-token-to (iterator pos)
  (setf (token-iterator-pos iterator) pos))

(defun seek-token-by (iterator offset)
  (with-slots (buffer pos) iterator
    (let ((new-pos (+ offset pos)))
      (if* (< new-pos 0)
         then (setf pos 0)
         else (setf pos new-pos)))))

(defstruct yaml-mark
  line
  column)

(defstruct (yaml-node (:print-function print-yaml-node))
  tree
  start
  end
  value)

(defun print-yaml-node (node stream depth)
  (declare (ignore depth))
  (print-unreadable-object (node stream :type t :identity t)))

(defstruct (yaml-document (:include yaml-node))
  directive)

(defstruct (yaml-mapping (:include yaml-node)))

(defstruct (yaml-sequence (:include yaml-node)))

(defstruct (yaml-scalar (:include yaml-node)))

(defstruct yaml-tree
  source
  tokens
  marks
  docs)

(defstruct yaml-parser
  tree
  token-it)

(declaim (ftype (function (yaml-parser fixnum) (or nil string)) get-directive))
(defun get-directive (parser doc-index)
  (with-slots (tree) parser
    #+yaml.debug (assert (< doc-index (length (yaml-tree-docs tree))))
    (let ((doc (aref (yaml-tree-docs tree) doc-index)))
      (when (yaml-document-p doc)
        (let ((id (yaml-document-directive doc)))
          (when id
            (get-raw parser id id)))))))

(declaim (ftype (function (yaml-parser fixnum fixnum) simple-string) get-raw))
(defun get-raw (parser start end)
  (with-slots (source tokens) (yaml-parser-tree parser)
    #+yaml.debug (assert (<= start end))
    #+yaml.debug (assert (and (< start (length tokens))
                              (< end (length tokens))))
    (let ((start-token (aref tokens start))
          (end-token (aref tokens end)))
      (subseq source (token-start start-token) (token-end end-token)))))

(defun parse-yaml (source)
  (let* ((tokens (make-array 0 :element-type 'token :adjustable t :fill-pointer 0))
         (token-it (make-token-iterator :buffer tokens :pos 0))
         (marks (make-hash-table :test 'eql))
         (docs (make-array 0 :element-type 'yaml-node :adjustable t :fill-pointer 0))
         (tree (make-yaml-tree :source source
                               :tokens tokens
                               :marks marks
                               :docs docs))
         (parser (make-yaml-parser :tree tree :token-it token-it)))

    ;; Start lexing
    (do* ((line 0)
          (prev-line-last-col 0)
          (lexer (make-lexer :source source))
          (tok (lex lexer) (lex lexer))
          (tok-idx (length tokens) (length tokens)))
         (() nil)
      (vector-push-extend tok tokens)
      (setf (gethash tok-idx marks)
            (make-yaml-mark :line   line
                            :column prev-line-last-col))
      (case (token-id tok)
        (eof (return))
        (new-line
         (incf line)
         (setq prev-line-last-col (token-end tok)))))

    ;; Start parsing
    (consume-comments-and-space parser '())
    (while t
      (consume-comments-and-space parser '())
      (let ((token (next-token token-it)))
        (when (not token) (return))
        (if* (eq 'eof (token-id token))
           then (return)
           else (seek-token-by token-it -1)
                (let ((doc (parse-yaml-document parser)))
                  (vector-push-extend doc docs)))))
    tree))

(defun parse-yaml-value (parser)
  (with-slots (token-it) parser
    (consume-comments-and-space parser '())
    (let ((pos (token-iterator-pos token-it))
          (token (next-token token-it)))
      (when (not token)
        (error 'yaml-unexpected-token-error))

      (case (token-id token)
        (literal
         (if* (consume-token parser 'map-value-ind '(new-line comment))
            then (seek-token-to token-it pos)
                 (parse-yaml-mapping parser)
            else (seek-token-to token-it pos)
                 (parse-yaml-scalar parser)))

        ((single-quoted double-quoted)
         (seek-token-by token-it -1)
         (parse-yaml-scalar parser))

        (seq-item-ind
         (seek-token-by token-it -1)
         (parse-yaml-sequence parser))

        (flow-seq-start
         (seek-token-by token-it -1)
         (parse-yaml-bracketed-sequence parser))

        (t nil)))))

(defun parse-yaml-document (parser)
  (with-slots (tree token-it) parser
    (let ((node (make-yaml-document :tree tree :start (token-iterator-pos token-it)))
          explicit-doc-p)
      ;; parse header
      (let ((doc-pos (consume-token parser 'doc-start '())))
        (if* doc-pos
           then (when (> (get-column parser doc-pos) 0)
                  (error 'yaml-malformed-error))
                (when (consume-token parser 'tag '(new-line comment))
                  (setf (yaml-document-directive node)
                        (expect-token parser 'literal '(new-line comment))))
                (setq explicit-doc-p t)
           else (setq explicit-doc-p nil)))

      ;; parse value
      (setf (yaml-node-value node) (parse-yaml-value parser))

      ;; parse footer
      (block nil
        (let (pos)
          (setq pos (consume-token parser 'doc-end '()))
          (when pos
            (when (not explicit-doc-p) (error 'yaml-unexpected-token-error))
            (when (> (get-column parser pos) 0) (error 'yaml-malformed-error))
            (setf (yaml-node-end node) pos)
            (return))

          (setq pos (consume-token parser 'doc-start '()))
          (when pos
            (when (not explicit-doc-p) (error 'yaml-unexpected-token-error))
            (when (> (get-column parser pos) 0) (error 'yaml-malformed-error))
            (seek-token-by token-it -1)
            (setf (yaml-node-end node) (- pos 1))
            (return))

          (setq pos (consume-token parser 'eof '()))
          (when pos
            (setf (yaml-node-end node) (- pos 1))
            (return)))
        (error 'yaml-unexpected-token-error))
      node)))

(defun parse-yaml-mapping (parser)
  (with-slots (tree token-it) parser
    (let* ((node (make-yaml-mapping :tree tree
                                    :start (token-iterator-pos token-it)
                                    :value (list)))
           (col (yaml-node-start node))
           key
           key-pos
           val)

      (while t
        (consume-comments-and-space parser '())

        ;; Parse key
        (setq key-pos (token-iterator-pos token-it))
        (when (< (get-column parser key-pos) col)
          (return))

        (setq key (next-token token-it))
        (case (token-id key)
          (literal nil)
          ((doc-start doc-end eof)
           (seek-token-by token-it -1)
           (return))
          (t
           ;; TODO: key not being a literal
           (error 'yaml-unhandled-error)))

        (expect-token parser 'map-value-ind '(new-line comment))

        ;; Parse value
        (setq val (parse-yaml-value parser))
        (when val
          (cond ((< (get-column parser (yaml-node-start val))
                    (get-column parser key-pos))
                 (error 'yaml-malformed-error))
                ((yaml-scalar-p val)
                 (when (= (get-column parser (yaml-scalar-start val))
                          (get-column parser key-pos))
                   (error 'yaml-malformed-error)))))

        (push (vector key-pos val) (yaml-node-value node))
        ) ;; end of while loop

      (setf (yaml-node-end node) (- (token-iterator-pos token-it) 1)
            (yaml-node-value node) (make-array (length (yaml-node-value node))
                                               :element-type t
                                               :initial-contents (nreverse (yaml-node-value node))))
      node)))

(defun parse-yaml-sequence (parser)
  (with-slots (tree token-it) parser
    (let ((node (make-yaml-sequence :tree tree
                                    :start (token-iterator-pos token-it)
                                    :value (list)))
          val)

      (while t
        (consume-comments-and-space parser '())
        (when (not (consume-token parser 'seq-item-ind '()))
          (return))
        (setq val (parse-yaml-value parser))
        (when (not val)
          (error 'yaml-malformed-error))
        (push val (yaml-node-value node)))

      (setf (yaml-node-end node) (- (token-iterator-pos token-it) 1)
            (yaml-node-value node) (make-array (length (yaml-node-value node))
                                               :element-type 't
                                               :initial-contents (nreverse (yaml-node-value node))))
      node)))

(defun parse-yaml-bracketed-sequence (parser)
  (with-slots (tree token-it) parser
    (let ((node (make-yaml-sequence :tree tree
                                    :start (token-iterator-pos token-it)
                                    :value (list)))
          val)

      (expect-token parser 'flow-seq-start '())

      (while t
        (consume-comments-and-space parser '(comment))
        (let ((pos (consume-token parser 'flow-seq-end '(comment))))
          (when pos
            (setf (yaml-node-end node) pos)
            (return)))
        (consume-token parser 'comma '(comment))
        (setq val (parse-yaml-value parser))
        (when (not val)
          (error 'yaml-malformed-error))
        (push val (yaml-node-value node)))

      (setf (yaml-node-value node)
            (make-array (length (yaml-node-value node))
                        :element-type 't
                        :initial-contents (nreverse (yaml-node-value node))))
      node)))

(defun parse-yaml-scalar (parser)
  (with-slots (tree token-it) parser
    (let ((node (make-yaml-scalar :tree tree
                                  :start (token-iterator-pos token-it)
                                  :value "")))
      ;; TODO: handle multiline strings in new block scope
      (do ((tok (next-token token-it) (next-token token-it)))
          ((not tok))
        (case (token-id tok)
          (single-quoted
           (setf (yaml-scalar-end node) (- (token-iterator-pos token-it) 1))
           (parse-single-quoted parser
                                node
                                (get-raw parser (yaml-scalar-start node) (yaml-scalar-end node)))
           (return))

          (double-quoted
           (setf (yaml-scalar-end node) (- (token-iterator-pos token-it) 1))
           (parse-double-quoted parser
                                node
                                (get-raw parser (yaml-scalar-start node) (yaml-scalar-end node)))
           (return))

          (literal nil)                 ; continue

          (space
           (let ((trailing (- (token-iterator-pos token-it) 2)))
             (consume-comments-and-space parser '())
             (let ((peek (peek-token token-it)))
               (when (and peek (not (eq 'literal (token-id peek))))
                 (setf (yaml-scalar-end node) trailing)
                 (setf (yaml-scalar-value node)
                       (string+ (yaml-scalar-value node)
                                (get-raw parser (yaml-scalar-start node) (yaml-scalar-end node))))
                 (return)))))
          (t
           (seek-token-by token-it -1)
           (setf (yaml-scalar-end node) (- (token-iterator-pos token-it) 1))
           (setf (yaml-scalar-value node)
                 (string+ (yaml-scalar-value node)
                          (get-raw parser (yaml-scalar-start node) (yaml-scalar-end node))))
           (return))

          ))                            ; end of do loop

      node)))

(defun consume-comments-and-space (parser exclusions)
  (with-slots (token-it) parser
    (let ((token (next-token token-it)))
      (while token
        (case (token-id token)
          ((comment space new-line)
           (dolist (exclu exclusions)
             (when (eq exclu (token-id token))
               (seek-token-by token-it -1)
               (return-from consume-comments-and-space))))
          (t
           (seek-token-by token-it -1)
           (return-from consume-comments-and-space)))
        (setq token (next-token token-it))))))

(defun consume-token (parser token-id exclusions)
  (with-slots (token-it) parser
    (consume-comments-and-space parser exclusions)
    (let ((pos (token-iterator-pos token-it))
          (token (next-token token-it)))
      (when token
        (if* (eq token-id (token-id token))
           then pos
           else (seek-token-by token-it -1)
                nil)))))

(defun expect-token (parser token-id exclusions)
  (when (not (consume-token parser token-id exclusions))
    (error 'yaml-unexpected-token-error)))

(defun get-line (parser idx)
  (with-slots (marks) (yaml-parser-tree parser)
    (yaml-mark-line (gethash idx marks))))

(defun get-column (parser idx)
  (with-slots (marks) (yaml-parser-tree parser)
    (yaml-mark-column (gethash idx marks))))

(defun parse-single-quoted (parser node raw)
  (declare (ignore parser))
  #+yaml.debug (assert (char= #\' (char raw 0) (char raw (1- (length raw)))))
  (do ((raw-no-quotes (subseq raw 1 (1- (length raw))))
       (state 'start)
       (i 0 (+ 1 i)))
      ((>= i (length raw-no-quotes)))
    (let ((c (char raw-no-quotes i)))
      (ecase state
        (start (if* (char= c #\')
                  then (setq state 'escape)
                  else (setf (yaml-scalar-value node)
                             (string+ (yaml-scalar-value node) c))))
        (escape (if* (char= c #\')
                   then (setq state 'start)
                        (setf (yaml-scalar-value node)
                              (string+ (yaml-scalar-value node) c))
                   else (error 'yaml-invalid-escape-sequence-error)))))))

(defun parse-double-quoted (parser node raw)
  (declare (ignore parser))
  #+yaml.debug (assert (char= #\" (char raw 0) (char raw (1- (length raw)))))
  (do ((raw-no-quotes (subseq raw 1 (1- (length raw))))
       (state 'start)
       (i 0 (+ 1 i)))
      ((>= i (length raw-no-quotes)))
    (let ((c (char raw-no-quotes i)))
      (ecase state
        (start (if* (char= c #\\)
                  then (setq state 'escape)
                  else (setf (yaml-scalar-value node)
                             (string+ (yaml-scalar-value node) c))))
        (escape (case c
                  (#\n
                   (setq state 'start)
                   (setf (yaml-scalar-value node)
                         (string+ (yaml-scalar-value node) #\Newline)))

                  (#\t
                   (setq state 'start)
                   (setf (yaml-scalar-value node)
                         (string+ (yaml-scalar-value node) #\Tab)))

                  (#\"
                   (setq state 'start)
                   (setf (yaml-scalar-value node)
                         (string+ (yaml-scalar-value node) #\")))

                  (t
                   (error 'yaml-invalid-escape-sequence-error))))))))

#+yaml.test
