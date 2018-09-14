(defpackage #:lsx/reader
  (:use #:cl)
  (:import-from #:lsx/component
                #:h)
  (:import-from #:lsx/html
                #:make-declaration-element)
  (:export #:enable-lsx-syntax
           #:disable-lsx-syntax))
(in-package #:lsx/reader)

(defun read-as-string (stream while)
  (let ((buffer (make-string 50)))
    (loop for i from 0
          for next = (peek-char nil stream)
          while (funcall while next)
          do (setf (aref buffer i) (read-char stream))
          when (<= (length buffer) (1+ i))
          do (let ((new-buffer (make-string (* 2 (length buffer)))))
               (replace new-buffer buffer)
               (setf buffer new-buffer))
          finally
             (return (subseq buffer 0 i)))))

(defun read-element-name (stream)
  (read-as-string stream
                  (lambda (char)
                    (or (alphanumericp char)
                        (find char '(#\- #\_ #\:))))))

(defun space-char-p (char)
  (find char '(#\Space #\Tab #\Linefeed #\Return #\Page)))

(defun skip-while (stream while)
  (loop while (funcall while (peek-char nil stream))
        do (read-char stream)))

(defun read-attribute-key (stream)
  (skip-while stream #'space-char-p)
  (read-as-string stream
                  (lambda (char)
                    (and (not (find char '(#\Null #\" #\' #\> #\/ #\=)))
                         (not (space-char-p char))))))

(defvar *default-readtable*)

(defun inline-lisp-reader (stream char)
  (declare (ignore char))
  `(lambda () ,@(let ((*readtable* (copy-readtable *default-readtable*)))
                  (set-syntax-from-char #\} #\))
                  (read-delimited-list #\} stream t))))

(defun read-attribute-value (stream)
  (let ((*default-readtable* *readtable*)
        (*readtable* (copy-readtable)))
    (set-macro-character #\{ #'inline-lisp-reader)
    (set-syntax-from-char #\} #\))
    (set-syntax-from-char #\' #\")
    (read stream)))

(defun read-attribute (stream)
  (list (read-attribute-key stream)
        (if (char= (peek-char t stream) #\=)
            (progn
              (read-char stream)
              (read-attribute-value stream))
            nil)))

(defvar *reading-tag*)
(defvar *reading-tag-children*)

(defun read-html-tag-inner (stream)
  (let ((next (peek-char nil stream)))
    (case next
      ((#\{ #\<)
       (push (read stream) *reading-tag-children*))
      (otherwise
       (push (read-as-string stream
                             (lambda (char)
                               (not (find char '(#\< #\{)))))
             *reading-tag-children*)))
    (loop
      (push (read-html-tag-inner stream) *reading-tag-children*))))

(defun read-html-tag (stream char)
  (declare (ignore char))
  (let ((next (peek-char nil stream)))
    (cond
      ((alphanumericp next)
       ;; Reading opening tag
       (let ((name (read-element-name stream))
             (attrs (loop until (find (peek-char t stream) '(#\/ #\>))
                          collect `(cons ,@(read-attribute stream)))))
         (let ((next (peek-char t stream)))
           (if (char= next #\/)
               ;; self closing tag
               (progn
                 (read-char stream)
                 (assert (char= (read-char stream) #\>))
                 (list 'h name `(list ,@attrs)))
               (let ((*reading-tag* name)
                     (*reading-tag-children* (list))
                     (*default-readtable* *readtable*)
                     (*readtable* (copy-readtable)))
                 (assert (char= (read-char stream) #\>))
                 (set-macro-character #\{ #'inline-lisp-reader)
                 (set-syntax-from-char #\} #\))
                 (list 'h name `(list ,@attrs)
                       (progn
                         (catch 'end-of-tag
                           (read-html-tag-inner stream))
                         `(list
                           ,@(nreverse *reading-tag-children*)))))))))
      ((char= next #\!)
       ;; Reading declaration
       (read-char stream)
       (let ((name (read-element-name stream)))
         (skip-while stream #'space-char-p)
         (let ((content (read-as-string stream
                                        (lambda (char)
                                          (not (char= char #\>))))))
           (assert (char= (read-char stream) #\>))
           `(make-declaration-element
             :name ,name
             :content ,content))))
      ((char= next #\/)
       ;; Reading closing tag
       (read-char stream)
       (let ((name (read-element-name stream)))
         (assert (char= (read-char stream) #\>))
         (unless (equal name *reading-tag*)
           (error "Unmatched closing tag: ~A" name)))
       (throw 'end-of-tag *reading-tag-children*))
      (t (intern (format nil "<~S" (read stream)))))))

(defun do-nothing (stream char)
  (declare (ignore stream char)))

(defvar *previous-readtables* '())

(defun %enable-lsx-syntax ()
  (push *readtable* *previous-readtables*)
  (setf *readtable* (copy-readtable))
  (set-macro-character #\< #'read-html-tag)
  (set-macro-character #\> #'do-nothing)
  (values))

(defun %disable-lsx-syntax ()
  (if *previous-readtables*
      (setf *readtable* (pop *previous-readtables*))
      (setf *readtable* (copy-readtable nil)))
  (values))

(defmacro enable-lsx-syntax ()
  `(eval-when (:compile-toplevel :load-toplevel :execute)
     (%enable-lsx-syntax)))

(defmacro disable-lsx-syntax ()
  `(eval-when (:compile-toplevel :load-toplevel :execute)
     (%disable-lsx-syntax)))
