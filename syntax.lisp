(in-package #:cl-user)

(syntax:define-package-syntax :lsx
  (:merge :standard)
  (:macro-char #\< #'lsx::read-html-tag))
