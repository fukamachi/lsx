(uiop:define-package #:lsx
  (:nicknames #:lsx/main)
  (:use #:cl)
  (:use-reexport #:lsx/reader
                 #:lsx/html
                 #:lsx/tag
                 #:lsx/file))
(in-package #:lsx)
