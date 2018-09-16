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

(defvar *escape-map*
  (let ((hash (make-hash-table)))
    (loop for (char . escaped) in '((#\& . "&amp;")
                                    (#\< . "&lt;")
                                    (#\> . "&gt;")
                                    (#\" . "&quot;")
                                    (#\' . "&#39;"))
          do (setf (gethash char hash) escaped))
    hash))

(defun print-escaped-text (value stream)
  (declare (type string value))
  (loop for char of-type character across value
        for escaped = (gethash char *escape-map*)
        if escaped
          do (write-string escaped stream)
        else do (write-char char stream)))

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

(defgeneric render-object (object stream)
  (:method (object stream)
    (print-escaped-text (princ-to-string object) stream))
  (:method ((object cons) stream)
    (loop for object in object
          do (fresh-line stream)
             (render-object object stream))))

(defmethod render-object ((element element) stream)
  (with-slots (name attributes children) element
    (format stream "<~A" name)
    (dolist (attr attributes)
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
    (let ((value (typecase value
                   (function (funcall value))
                   (otherwise value))))
      (when value
        (write-char #\Space stream)
        (format stream "~A" name)
        (unless (eq value t)
          (flet ((write-value (value)
                   (write-char #\= stream)
                   (write-char #\" stream)
                   (render-object value stream)
                   (write-char #\" stream)))
            (write-value value)))))))

(defmethod render-object ((object element-list) stream)
  (dolist (element (element-list-elements object))
    (render-object element stream)
    (write-char #\Newline stream)))

(defmethod render-object ((object declaration-element) stream)
  (with-slots (name content) object
    (format stream "<!~A ~A>"
            name content)))

(defmethod render-object ((object string) stream)
  (print-escaped-text object stream))

(defmethod render-object ((object number) stream)
  (print-escaped-text (write-to-string object) stream))

(defmethod render-object ((object null) stream)
  (declare (ignore stream)))

(defmethod render-object ((object (eql 't)) stream)
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
