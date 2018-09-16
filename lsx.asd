(defsystem "lsx"
  :class :package-inferred-system
  :version "0.1.0"
  :author "Eitaro Fukamachi"
  :license "BSD 2-Clause"
  :description "Embeddable HTML templating engine with JSX-like syntax"
  :depends-on ("lsx/main"))

(defsystem "lsx/tests"
  :class :package-inferred-system
  :depends-on ("rove"
               "lsx/tests/main")
  :perform (test-op (o c) (symbol-call :rove '#:run c)))
