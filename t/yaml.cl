;;;; test-yaml.cl
(eval-when (:compile-toplevel :load-toplevel :execute)
  (require :tester))

(defpackage #:yaml.test
  (:use #:cl
        #:excl
        #:yaml
        #:util.string
        #:util.test)
  (:export #:main))

(in-package #:yaml.test)

(eval-when (:load-toplevel :execute)
  (defparameter *test-data-directory*
    (string+ (directory-namestring *load-pathname*) "data/")))

(defun test-construct-bool ()
  (let ((actual (read-yaml (string+ *test-data-directory* "construct-bool.yaml") :pathname-p t)))
    (test '(t t) (gethash "canonical" actual) :test 'eq :multiple-values t)
    (test '(nil t) (gethash "answer" actual) :test 'eq :multiple-values t)
    (test '(t t) (gethash "logical" actual) :test 'eq :multiple-values t)
    (test '(t t) (gethash "option" actual) :test 'eq :multiple-values t)
    (test '("is a string" t) (gethash t (gethash "but" actual)) :test 'equal :multiple-values t)
    (test '("is a string" t) (gethash nil (gethash "but" actual)) :test 'equal :multiple-values t)))

(defun test-construct-float ()
  (let ((actual (read-yaml (string+ *test-data-directory* "construct-float.yaml") :pathname-p t)))
    (test '(685230.15d0 t) (gethash "canonical" actual) :test 'eql :multiple-values t)
    (test '(68523015000d0 t) (gethash "exponential" actual) :test 'eql :multiple-values t)
    (test '(685230.15d0 t) (gethash "fixed" actual) :test 'eql :multiple-values t)
    (test t (infinityp (gethash "negative infinity" actual)))
    (test t (and (nanp (gethash "not a number" actual)) t))))

(defun test-read-yaml ()
  (with-tests (:name "test-read-yaml")
    (test-construct-bool)
    (test-construct-float)))

(defun test-write-yaml ())

(defun main ()
  (let ((*break-on-test-failures* nil)
        (*error-protect-tests* t))
    (test-read-yaml)
    (test-write-yaml)))
