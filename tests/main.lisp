(defpackage #:lsx/tests/main
  (:use #:cl
        #:rove
        #:lsx/component
        #:lsx/html
        #:lsx/file)
  (:import-from #:lsx/html
                #:print-escaped-text)
  (:import-from #:local-time
                #:now)
  (:import-from #:cl-ppcre))
(in-package #:lsx/tests/main)

(defun esc (v)
  (with-output-to-string (s)
    (print-escaped-text v s)))

(deftest escaped-text-tests
  (ok (equal (esc "Hello") "Hello")
      "Normal string")
  (ok (equal (esc "Tiffany & Co.") "Tiffany &amp; Co.")
      "Escape &")
  (ok (equal (esc "<danger>") "&lt;danger&gt;")
      "Escape <, >")
  (ok (equal (esc "\"LEVI'S\"") "&quot;LEVI&#39;S&quot;")
      "Escape \", \'"))

(deftest element-tests
  (testing "Normal element"
    (let ((br (eval (read-lsx-string "<br/>"))))
      (ok (typep br 'element))
      (ok (equal (element-name br) "br"))
      (ok (outputs (render-object br t) "<br>"))))
  (testing "With attributes & children"
    (let ((a (eval (read-lsx-string "<a href=\"/hello\">Say Hello</a>"))))
      (ok (typep a 'element))
      (ok (equal (element-name a) "a"))
      (ok (outputs (render-object a t) "<a href=\"/hello\">Say Hello</a>"))))
  (testing "Embed Lisp code"
    (let ((a (eval (read-lsx-string "<a href=\"/hello\">Say Hello at {(local-time:now)}</a>"))))
      (ok (typep a 'element))
      (ok (equal (element-name a) "a"))
      (ok (ppcre:scan
           "^<a href=\"/hello\">Say Hello at \\d{4}-\\d{2}-\\d{2}T\\d{2}:\\d{2}:\\d{2}(\\.\\d{6})?.*</a>$"
           (with-output-to-string (s)
             (render-object a s)))))))

(eval-when (:compile-toplevel :load-toplevel :execute)
  (when (find-class 'welcome nil)
    (setf (find-class 'welcome) nil)))

(deftest custom-component-tests
  (defcomponent welcome ()
    (name)
    (:render (h "h1" () (list (lambda () name)))))

  (let ((welcome (eval (read-lsx-string "<welcome name=\"fukamachi\" />"))))
    (ok (typep welcome 'welcome))
    (ok (outputs (render-object welcome t) "<h1>fukamachi</h1>"))))
