;;;; parser.cl
(in-package #:yaml)

(define-condition yaml-parse-error (simple-error)
  ())

(define-condition yaml-invalid-escape-sequence (yaml-parse-error)
  ())

(define-condition yaml-malformed (yaml-parse-error)
  ())

(define-condition yaml-nested-documents (yaml-parse-error)
  ())

(define-condition yaml-unexpected-eof (yaml-parse-error)
  ())

(define-condition yaml-unexpected-token (yaml-parse-error)
  ())

(define-condition yaml-unhandled-error (yaml-parse-error)
  ())

(defstruct yaml-node
  yah
  tree
  start
  end)
