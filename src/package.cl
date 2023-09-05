;;;; package.cl
(in-package #:cl-user)

(defpackage #:yaml
  (:use #:cl
        #:excl
        #:ff)
  (:export #:*yaml-null*
           #:yaml-null-p
           #:read-yaml
           #:read-yaml*))
