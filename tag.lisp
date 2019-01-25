(defpackage #:lsx/tag
  (:use #:cl)
  (:import-from #:lsx/html
                #:make-element-list
                #:h*
                #:prologue)
  (:import-from #:alexandria
                #:with-gensyms)
  (:export #:deftag
           #:h))
(in-package #:lsx/tag)

(defvar *user-tags*
  (make-hash-table :test 'equal))

(defmacro deftag (name lambda-list &body body)
  (with-gensyms (tag-name)
    `(eval-when (:compile-toplevel :load-toplevel :execute)
       (let ((,tag-name (etypecase ',name
                          (symbol
                           (let ((*print-case* :downcase))
                             (princ-to-string ',name)))
                          (string ',name))))
         (setf (gethash ,tag-name *user-tags*)
               (lambda ,lambda-list
                 ,@body))))))

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
