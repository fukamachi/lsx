(defpackage #:lsx/component
  (:use #:cl)
  (:import-from #:lsx/html
                #:render-object
                #:make-element-list
                #:h*
                #:prologue)
  (:import-from #:closer-mop)
  (:export #:component
           #:component-class
           #:defcomponent
           #:h))
(in-package #:lsx/component)

(defclass component () ())

(defmethod render-object ((object component) stream)
  (let ((class (class-of object)))
    (apply (slot-value class 'render)
           (mapcan (lambda (slot)
                     (when (slot-boundp object (c2mop:slot-definition-name slot))
                       (list (intern (princ-to-string (c2mop:slot-definition-name slot)) :keyword)
                             (slot-value object (c2mop:slot-definition-name slot)))))
                   (c2mop:class-slots class)))))

(defun html-attributes-to-plist (attributes)
  (loop for (name . value) in attributes
        append (list (read-from-string (format nil ":~A" name)) value)))

(defun h (tag-name attributes &optional (children nil children-specified-p))
  (let* ((name (read-from-string tag-name))
         (component-class (find-class name nil)))
    (cond
      ((and component-class
            (c2mop:subclassp component-class 'component))
       (apply #'make-instance component-class
              :children children
              :allow-other-keys t
              (html-attributes-to-plist attributes)))
      ((string-equal name "html")
       (make-element-list
        :elements (list (prologue) (h* tag-name attributes children))))
      (t
       (if children-specified-p
           (h* tag-name attributes children)
           (h* tag-name attributes))))))

(defclass component-slot-class (c2mop:standard-direct-slot-definition)
  ())

(defmacro define-initialize-instance (method-qualifier lambda-list &body body)
  `(progn
     (defmethod initialize-instance ,method-qualifier ,lambda-list ,@body)
     (defmethod reinitialize-instance ,method-qualifier ,lambda-list ,@body)))

(define-initialize-instance :around ((class component-slot-class) &rest args &key name &allow-other-keys)
  (push
   (intern (princ-to-string name) :keyword)
   (getf args :initargs))
  (apply #'call-next-method class args))

(defclass component-class (standard-class)
  ((render :initarg :render
           :initform nil)))

(defmethod c2mop:direct-slot-definition-class ((class component-class) &key &allow-other-keys)
  'component-slot-class)

(defmethod c2mop:validate-superclass ((class component-class) (super standard-class))
  t)

(define-initialize-instance :around ((class component-class) &rest initargs &key direct-slots render &allow-other-keys)
  (when render
    (setf (getf initargs :render)
          (eval `(lambda (&key ,@(mapcar (lambda (slot)
                                           (getf slot :name))
                                  direct-slots)
                          &allow-other-keys)
                   (mapc (lambda (element)
                           (render-object element t))
                         (list ,@render))
                   nil))))
  (apply #'call-next-method class initargs))

(defmacro defcomponent (name superclasses slot-definitions &rest class-options)
  `(defclass ,name (component ,@superclasses)
     ,slot-definitions
     (:metaclass component-class)
     ,@class-options))
