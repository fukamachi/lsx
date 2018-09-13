(defpackage #:lsx/component
  (:use #:cl)
  (:import-from #:lsx/html
                #:render-object
                #:h*)
  (:import-from #:closer-mop)
  (:export #:component
           #:h))
(in-package #:lsx/component)

(defclass component () ())

(defmethod render-object ((object component) stream)
  (error "No render-object method for ~A" (type-of object)))

(defun html-attributes-to-plist (attributes)
  (loop for (name . value) in attributes
        append (list (read-from-string (format nil ":~A" name)) value)))

(defun h (tag-name attributes &optional (children nil children-specified-p))
  (let* ((name (read-from-string tag-name))
         (component-class (find-class name nil)))
    (if (and component-class
             (c2mop:subclassp component-class 'component))
        (apply #'make-instance component-class
               :children children
               :allow-other-keys t
               (html-attributes-to-plist attributes))
        (if children-specified-p
            (h* tag-name attributes children)
            (h* tag-name attributes)))))
