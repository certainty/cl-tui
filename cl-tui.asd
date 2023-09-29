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
  :in-order-to ((test-op (test-op :cli-tui/test))))

(defsystem "cl-tui/test"
  :pathname "tests"
  :defsystem-depends-on ("fiveam-asdf")
  :depends-on ("cl-tui" "fiveam")
  :serial t
  :components ((:file "test")))
