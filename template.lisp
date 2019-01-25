(defpackage #:lsx/template
  (:use #:cl)
  (:import-from #:lsx/html
                #:render-object)
  (:import-from #:closer-mop)
  (:export #:template
           #:template-class
           #:deftemplate
           #:render))
(in-package #:lsx/template)

(defclass template () ())

(defmethod render-object ((object template) stream)
  (let ((class (class-of object)))
    (funcall (slot-value class 'render) stream object)))

(defclass template-slot-class (c2mop:standard-direct-slot-definition)
  ())

(defmacro define-initialize-instance (method-qualifier lambda-list &body body)
  `(progn
     (defmethod initialize-instance ,method-qualifier ,lambda-list ,@body)
     (defmethod reinitialize-instance ,method-qualifier ,lambda-list ,@body)))

(define-initialize-instance :around ((class template-slot-class) &rest args &key name &allow-other-keys)
  (push
   (intern (princ-to-string name) :keyword)
   (getf args :initargs))
  (apply #'call-next-method class args))

(defclass template-class (standard-class)
  ((render :initarg :render
           :initform nil)))

(defmethod c2mop:direct-slot-definition-class ((class template-class) &key &allow-other-keys)
  'template-slot-class)

(defmethod c2mop:validate-superclass ((class template-class) (super standard-class))
  t)

(define-initialize-instance :after ((class template-class) &rest initargs &key direct-slots render &allow-other-keys)
  (declare (ignore initargs))
  (when render
    (let* ((stream (gensym "STREAM"))
           (object (gensym "OBJECT"))
           (slot-names (mapcar (lambda (slot) (getf slot :name))
                               direct-slots))
           (generic-function (ensure-generic-function 'render-object :lambda-list '(object stream)))
           (main-fn (eval
                     `(lambda (,stream ,object)
                        (with-slots (,@slot-names) ,object
                          (declare (ignorable ,@slot-names))
                          (mapc (lambda (element)
                                  (render-object element ,stream))
                                (list ,@render))
                          (values))))))
      (add-method generic-function
                  (make-instance 'standard-method
                                 :lambda-list '(object stream)
                                 :qualifiers ()
                                 :specializers (list class (find-class 't))
                                 :function
                                 (lambda (args &rest ignore)
                                   (declare (ignore ignore))
                                   (destructuring-bind (object stream) args
                                     (funcall main-fn stream object))))))))

(defmacro deftemplate (name superclasses slot-definitions &rest class-options)
  `(defclass ,name (,@superclasses template)
     ,slot-definitions
     (:metaclass template-class)
     ,@class-options))

(defgeneric render (template &rest args)
  (:method ((template template-class) &rest args)
    (render-object (apply #'make-instance template
                          :allow-other-keys t
                          args)
                   nil))
  (:method ((template symbol) &rest args)
    (apply #'render (find-class template) args)))
