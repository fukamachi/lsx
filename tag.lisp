(defpackage #:lsx/tag
  (:use #:cl)
  (:import-from #:lsx/html
                #:make-element-list
                #:h*
                #:prologue)
  (:export #:deftag
           #:h))
(in-package #:lsx/tag)

(defvar *user-tags*
  (make-hash-table :test 'eq))

(defmacro deftag (name lambda-list &body body)
  (check-type name symbol)
  `(eval-when (:compile-toplevel :load-toplevel :execute)
     (setf (gethash ',name *user-tags*)
           (lambda ,lambda-list
             ,@body))))

(defun html-attributes-to-plist (attributes)
  (loop for (name . value) in attributes
        append (list (read-from-string (format nil ":~A" name)) value)))

(defun h (tag-name &optional attributes (children nil children-specified-p))
  (cond
    ((gethash tag-name *user-tags*)
     (apply (gethash tag-name *user-tags*)
            :children children
            :allow-other-keys t
            (html-attributes-to-plist attributes)))
    ((string-equal tag-name "html")
     (make-element-list
      :elements (list (prologue) (h* tag-name attributes children))))
    (t
     (if children-specified-p
         (h* tag-name attributes children)
         (h* tag-name attributes)))))
