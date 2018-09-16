(defpackage #:lsx/file
  (:use #:cl)
  (:import-from #:lsx/reader
                #:enable-lsx-syntax)
  (:import-from #:lsx/html
                #:make-element-list)
  (:export #:read-lsx-file))
(in-package #:lsx/file)

(defun read-lsx-file (file)
  (let ((*package* *package*)
        (*readtable* (copy-readtable))
        (*load-pathname* file)
        (*load-truename* file))
    (enable-lsx-syntax)
    (let ((forms
            (uiop:read-file-forms file)))
      (if (and (not (rest forms))
               (consp (first forms))
               (eq 'cl:lambda (first (first forms))))
          (eval (eval (first forms)))
          (make-element-list :elements (mapcar #'eval forms))))))
