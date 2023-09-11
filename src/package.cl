;;;; package.cl
(in-package #:cl-user)

(defpackage #:yaml
  (:use #:cl
        #:excl
        #:ff
        #:util.string)
  (:export #:*yaml-null*
           #:yaml-null-p
           #:read-yaml))
