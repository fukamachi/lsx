(defpackage #:lsx/file
  (:use #:cl)
  (:import-from #:lsx/reader
                #:enable-lsx-syntax)
  (:export #:read-lsx-file))
(in-package #:lsx/file)

(defun read-lsx-file (file)
  (let ((*package* *package*)
        (*readtable* (copy-readtable))
        (*load-pathname* file)
        (*load-truename* file))
    (enable-lsx-syntax)
    (let (result)
      (dolist (form (uiop:read-file-forms file) result)
        (setf result (eval form))))))
