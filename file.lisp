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
    (make-element-list
     :elements (mapcar #'eval
                       (uiop:read-file-forms file)))))
