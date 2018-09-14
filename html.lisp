(defpackage #:lsx/html
  (:use #:cl)
  (:export #:h*
           #:render-object
           #:html-mode
           #:element
           #:element-list
           #:declaration-element
           #:make-element
           #:element-name
           #:element-attributes
           #:element-children
           #:element-self-closing
           #:attribute
           #:attribute-name
           #:attribute-value))
(in-package #:lsx/html)

(defvar *html-mode* :html5)

(defun html-mode () *html-mode*)
(defun (setf html-mode) (new-value)
  (ecase *html-mode*
    ((:html :xhtml :html5) (setf *html-mode* new-value))))

(defstruct element
  (name nil :type string)
  (attributes nil :type list)
  (children nil :type list)
  (self-closing nil :type boolean))

(defstruct element-list
  (elements nil :type list))

(defstruct (declaration-element (:include element (name)))
  (content nil :type string))

(defstruct attribute
  (name nil :type string)
  value)

(defmethod print-object ((object element) stream)
  (print-unreadable-object (object stream :type t :identity t)
    (princ (element-name object) stream)))

(defmethod print-object ((object element-list) stream)
  (print-unreadable-object (object stream :type t :identity t)
    (format stream "(~D ~:*element~[s~;~:;s~])"
            (length (element-list-elements object)))))

(defmethod render-object ((object element-list) stream)
  (dolist (element (element-list-elements object))
    (render-object element stream)))

(defmethod render-object ((object declaration-element) stream)
  (with-slots (name content) object
    (format stream "<!~A ~A>"
            name content)))

(defgeneric render-object (object stream)
  (:method (object stream)
    (princ object stream)))

(defmethod render-object ((element element) stream)
  (with-slots (name attributes children) element
    (format stream "<~A" name)
    (dolist (attr attributes)
      (write-char #\Space stream)
      (render-object attr stream))
    (if (element-self-closing element)
        (case (html-mode)
          ((:xml :xhtml)
           (write-string " />" stream))
          (otherwise
           (write-char #\> stream)))
        (progn
          (write-char #\> stream)
          (loop for (child . rest) on children
                do (render-object child stream)
                   (when (and (stringp child)
                              (stringp (first rest)))
                     (write-char #\Space stream)))
          (format stream "</~A>" name)))
    nil))

(defmethod render-object ((attribute attribute) stream)
  (with-slots (name value) attribute
    (format stream "~A" name)
    (flet ((write-value (value)
             (write-char #\= stream)
             (write-char #\" stream)
             (typecase value
               (function (render-object (funcall value) stream))
               (otherwise (render-object value stream)))
             (write-char #\" stream)))
      (when value
        (write-value value)))))

(defmethod render-object ((object string) stream)
  (write-string object stream))

(defmethod render-object ((object number) stream)
  (princ object stream))

(defmethod render-object ((object null) stream)
  (declare (ignore stream)))

(defmethod render-object ((object function) stream)
  (render-object (funcall object) stream))

(defun h* (tag-name attributes &optional (children nil children-specified-p))
  (make-element
   :name tag-name
   :attributes (loop for (name . value) in attributes
                     collect (make-attribute :name name :value value))
   :children children
   :self-closing (not children-specified-p)))
