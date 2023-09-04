;;;; pkg.cl
(in-package #:cl-user)

(defpackage #:yaml.pkg
  (:use #:cl
        #:excl
        #:util.string)
  (:export #:compile-and-load-yaml
           #:build-yaml))

(in-package #:yaml.pkg)

(eval-when (:load-toplevel :execute)
  (defparameter *yaml-src-files*
                (let ((files '(;; start of sys module
                               "src/package"
                               "src/ffi"
                               "src/yaml"
                               ;; files list ends here
                               )))
                  (mapcar (lambda (file)
                            (string+ (directory-namestring *load-pathname*) file))
                          files)))

  (defparameter *yaml-fasl-output*
                (string+ (directory-namestring *load-pathname*) "yaml.fasl")))

(defun compile-and-load-yaml ()
  (dolist (file *yaml-src-files*)
    (compile-file (string+ file ".cl")
                  :load-after-compile t)))

(defun build-yaml ()
  (compile-and-load-yaml)
  (with-open-file (out *yaml-fasl-output* :direction :output
                                          :if-exists :supersede
                                          :if-does-not-exist :create)
    (dolist (file *yaml-src-files*)
      (sys:copy-file (string+ file ".fasl") out))))