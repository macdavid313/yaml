;;;; tokenizer.cl
(in-package #:yaml)

(defstruct lexer
  (source  "" :type string)
  (index 0 :type fixnum))

(deftype token-id ()
  '(member
    ;; List of all possible token ids.
    eof

    new-line
    doc-start                           ; ---
    doc-end                             ; ...
    seq-item-ind                        ; -
    map-value-ind                       ; :
    flow-map-start                      ; {
    flow-map-end                        ; }
    flow-seq-start                      ; [
    flow-seq-end                        ; ]

    comma
    space
    tab
    comment                             ; #
    alias                               ; *
    anchor                              ; &
    tag                                 ; !

    single-quoted                       ; '...'
    double-quoted                       ; "..."
    literal))

(defstruct token
  (id    'eof :type token-id)
  (start 0    :type fixnum)
  (end   -1   :type fixnum))

(defstruct token-iterator
  (buffer nil :type (array token (*)))
  (pos      0 :type fixnum))

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

(defun matches-pattern-p (pattern str offset)
  (let ((count 0)
        (slice (make-array (- (length str) offset) :element-type 'character
                                                   :displaced-to str
                                                   :displaced-index-offset offset)))
    (while (< count (length pattern))
      (when (>= count (length slice))
        (return-from matches-pattern-p nil))
      (when (char/= (char slice count)
                    (char pattern count))
        (return-from matches-pattern-p nil))
      (incf count))
    t))

(deftype lexer-state ()
  '(member
    ;; A list of all possible lexer states.
    start
    new-line
    space
    tab
    comment
    single-quoted
    double-quoted
    literal))

(defun lex (lexer)
  (declare (type lexer lexer))
  (with-slots (source index) lexer
    (let ((result (make-token :start index))
          (state 'start))               ; see type 'lexer-state
      (declare (type token result)
               (type lexer-state state))
      (while (< index (length source))
        (let ((c (char source index)))
          (case state
            ('start (case c
                      (#\Space
                       (setq state 'space))

                      (#\Tab
                       (setq state 'tab))

                      (#\Newline
                       (setf (token-id result) 'new-line)
                       (incf index)
                       (return))

                      (#\Return
                       (setq state 'new-line))

                      (#\-
                       (if* (matches-pattern-p "---" source index)
                          then (setf (token-id result) 'doc-start)
                               (incf index #.(length "---"))
                               (return)
                        elseif (matches-pattern-p "- " source index)
                          then (setf (token-id result) 'seq-item-ind)
                               (incf index #.(length "- "))
                               (return)
                          else (setq state 'literal)))

                      (#\.
                       (if* (matches-pattern-p "..." source index)
                          then (setf (token-id result) 'doc-end)
                               (incf index #.(length "..."))
                               (return)
                          else (setq state 'literal)))

                      (#\,
                       (setf (token-id result) 'comma)
                       (incf index)
                       (return))

                      (#\#
                       (setq state 'comment))

                      (#\*
                       (setf (token-id result) 'alias)
                       (incf index)
                       (return))

                      (#\&
                       (setf (token-id result) 'anchor)
                       (incf index)
                       (return))

                      (#\!
                       (setf (token-id result) 'tag)
                       (incf index)
                       (return))

                      (#\[
                       (setf (token-id result) 'flow-seq-start)
                       (incf index)
                       (return))

                      (#\]
                       (setf (token-id result) 'flow-seq-end)
                       (incf index)
                       (return))

                      (#\:
                       (setf (token-id result) 'map-value-ind)
                       (incf index)
                       (return))

                      (#\{
                       (setf (token-id result) 'flow-map-start)
                       (incf index)
                       (return))

                      (#\}
                       (setf (token-id result) 'flow-map-end)
                       (incf index)
                       (return))

                      (#\'
                       (setq state 'single-quoted))

                      (#\"
                       (setq state 'double-quoted))

                      (t
                       (setq state 'literal)))
             ) ;; end of 'start state

            (comment (when (or (char= c #\Return) (char= c #\Newline))
                       (setf (token-id result) 'comment)
                       (return)))

            (space (when (char/= c #\Space)
                     (setf (token-id result) 'space)
                     (return)))

            (tab (when (char/= c #\Tab)
                   (setf (token-id result) 'tab)
                   (return)))

            (new-line (when (char= c #\Newline)
                        (setf (token-id result) 'new-line)
                        (incf index)
                        (return)))

            (single-quoted (when (char= c #\')
                             (if* (not (matches-pattern-p "''" source index))
                                then (setf (token-id result) 'single-quoted)
                                     (incf index)
                                     (return)
                                else (incf index #.(- (length "''") 1)))))

            (double-quoted (when (char= c #\")
                             (if* (matches-pattern-p "\\" source (- index 1))
                                then (incf index)
                                else (setf (token-id result) 'double-quoted)
                                     (incf index)
                                     (return))))

            (literal (if* (member c '(#\Return #\Newline #\Space #\' #\" #\, #\: #\] #\}) :test 'char=)
                        then (setf (token-id result) 'literal)
                             (return)
                        else (setf (token-id result) 'literal)))))
        (incf index)) ;; end of while

      (when (and (>= index (length source))
                 (eq state 'literal))
        (setf (token-id result) 'literal))

      (setf (token-end result) index)
      result)))

#+yaml.test
(eval-when (:compile-toplevel :load-toplevel :execute)
  (defmacro test-expected-ids (str expected)
    (let ((lexer (gensym "lexer-")))
      `(let ((,lexer (make-lexer :source ,str)))
         (test ,expected
               (let (actual)
                 (loop
                   (let ((token (lex ,lexer)))
                     (push (token-id token) actual)
                     (when (eq (token-id token) 'eof)
                       (return))))
                 (nreverse actual))
               :test 'equal))))

  (defun test-empty-doc ()
    (test-expected-ids "" '(eof)))

  (defun test-empty-doc-with-explicit-markers ()
    (test-expected-ids "--- !tbd-v1
..."
                       '(doc-start
                         space
                         tag
                         literal
                         new-line
                         doc-end
                         eof)))

  (defun test-sequence-of-values ()
    (test-expected-ids "- 0
- 1
- 2"
                       '(seq-item-ind
                         literal
                         new-line
                         seq-item-ind
                         literal
                         new-line
                         seq-item-ind
                         literal
                         eof)))

  (defun test-sequence-of-sequences ()
    (test-expected-ids "- [ val1, val2]
- [val3, val4 ]"
                       '(seq-item-ind
                         flow-seq-start
                         space
                         literal
                         comma
                         space
                         literal
                         flow-seq-end
                         new-line
                         seq-item-ind
                         flow-seq-start
                         literal
                         comma
                         space
                         literal
                         space
                         flow-seq-end
                         eof)))

  (defun test-mappings ()
    (test-expected-ids "key1: value1
key2: value2"
                       '(literal
                         map-value-ind
                         space
                         literal
                         new-line
                         literal
                         map-value-ind
                         space
                         literal
                         eof)))

  (defun test-inline-mapped-sequence-of-values ()
    (test-expected-ids "key :  [ val1,
          val2 ]"
                       '(literal
                         space
                         map-value-ind
                         space
                         flow-seq-start
                         space
                         literal
                         comma
                         new-line
                         space
                         literal
                         space
                         flow-seq-end
                         eof)))

  (defun test-part-of-tbd ()
    (test-expected-ids "--- !tapi-tbd
tbd-version:     4
targets:         [ x86_64-macos ]

uuids:
  - target:          x86_64-macos
    value:           F86CC732-D5E4-30B5-AA7D-167DF5EC2708

install-name:    '/usr/lib/libSystem.B.dylib'
..."
                       '(doc-start
                         space
                         tag
                         literal
                         new-line
                         literal
                         map-value-ind
                         space
                         literal
                         new-line
                         literal
                         map-value-ind
                         space
                         flow-seq-start
                         space
                         literal
                         space
                         flow-seq-end
                         new-line
                         new-line
                         literal
                         map-value-ind
                         new-line
                         space
                         seq-item-ind
                         literal
                         map-value-ind
                         space
                         literal
                         new-line
                         space
                         literal
                         map-value-ind
                         space
                         literal
                         new-line
                         new-line
                         literal
                         map-value-ind
                         space
                         single-quoted
                         new-line
                         doc-end
                         eof)))

  (defun test-unindented-list ()
    (test-expected-ids "b:
- foo: 1
c: 1"
                       '(literal
                         map-value-ind
                         new-line
                         seq-item-ind
                         literal
                         map-value-ind
                         space
                         literal
                         new-line
                         literal
                         map-value-ind
                         space
                         literal
                         eof)))

  (defun test-escape-sequences ()
    (test-expected-ids "a: 'here''s an apostrophe'
b: \"a newline
and a  tab\"
c: \"\\\"here\\\" and there\""
                       '(literal
                         map-value-ind
                         space
                         single-quoted
                         new-line
                         literal
                         map-value-ind
                         space
                         double-quoted
                         new-line
                         literal
                         map-value-ind
                         space
                         double-quoted
                         eof)))

  (defun test-comments ()
    (test-expected-ids "key: # some comment about the key
# first value
- val1
# second value
- val2"
                       '(literal
                         map-value-ind
                         space
                         comment
                         new-line
                         comment
                         new-line
                         seq-item-ind
                         literal
                         new-line
                         comment
                         new-line
                         seq-item-ind
                         literal
                         eof)))

  (defun test-quoted-literals ()
    (test-expected-ids "'#000000'
'[000000'
\"&someString\""
                       '(single-quoted
                         new-line
                         single-quoted
                         new-line
                         double-quoted
                         eof)))

  (defun test-lexer ()
    (test-empty-doc)
    (test-empty-doc-with-explicit-markers)
    (test-sequence-of-values)
    (test-sequence-of-sequences)
    (test-mappings)
    (test-inline-mapped-sequence-of-values)
    (test-part-of-tbd)
    (test-unindented-list)
    (test-escape-sequences)
    (test-comments)
    (test-quoted-literals))

  ) ;; end of eval-when
