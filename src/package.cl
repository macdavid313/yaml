;;;; package.cl
(in-package #:cl-user)

(defpackage #:yaml
  (:use #:cl
        #:excl
        #:ff)
  (:export #:read-yaml
           #:read-yaml*))
