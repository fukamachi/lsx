(defpackage #:lsx/file
  (:use #:cl)
  (:import-from #:lsx/reader
                #:enable-lsx-syntax)
  (:export #:with-lsx-syntax
           #:read-lsx-string
           #:read-lsx-file))
(in-package #:lsx/file)

(defmacro with-lsx-syntax (&body body)
  `(let ((*package* *package*)
         (*readtable* (copy-readtable)))
     (enable-lsx-syntax)
     ,@body))

(defun read-lsx-string (string)
  (check-type string string)
  (with-lsx-syntax
    (read-from-string string)))

(defun read-lsx-file (file)
  (check-type file pathname)
  (let ((*load-pathname* file)
        (*load-truename* file))
    (with-lsx-syntax
      (let ((*package* *package*)
            (*readtable* (copy-readtable))
            result)
        (dolist (form (uiop:read-file-forms file) result)
          (setf result (eval form)))))))
