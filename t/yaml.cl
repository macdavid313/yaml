;;;; test-yaml.cl
(eval-when (:compile-toplevel :load-toplevel :execute)
  (require :tester))

(defpackage #:yaml.test
  (:use #:cl
        #:excl
        #:yaml
        #:util.test)
  (:export #:main))

(in-package #:yaml.test)

(defun main ()
  (with-tests (:name "test-read-yaml")
    (test 1 (read-yaml "1"))))
