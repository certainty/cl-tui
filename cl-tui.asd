(in-package :asdf-user)

(defsystem "cl-tui"
  :description "A library for building terminal user interfaces in Common Lisp."
  :author "certainty <david.krentzlin@gmail.com>"
  :version "0.0.1"
  :license "MIT"
  :source-control (:git "https://github.com/certainty/cl-tui.git")
  :pathname "src"
  :serial t
  :components ((:file "tui"))
  :in-order-to ((test-op (test-op :cl-tui/test))))

(defsystem "cl-tui/test"
  :pathname "tests"
  :depends-on (:cl-tui :rove)
  :serial t
  :components ((:file "test"))
  :perform (test-op (op c) (symbol-call :rove :run c)))
