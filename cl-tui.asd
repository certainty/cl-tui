
(asdf:defsystem "cl-tui"
  :description "A library for building terminal user interfaces in Common Lisp."
  :author "certainty <david.krentzlin@gmail.com>"
  :version "0.0.1"
  :license "MIT"
  :source-control (:git "https://github.com/certainty/cl-tui.git")
  :serial t
  :pathname "src"
  :depends-on (:cl-tui/terminal)
  :components ((:file "tui"))
  :in-order-to ((test-op (test-op  "cl-tui/tests"))))

(asdf:defsystem "cl-tui/terminal"
  :pathname "src/terminal"
  :depends-on (:cffi :cl-interpol)
  :defsystem-depends-on (:cffi-grovel)
  :pathname "src/terminal"
  :components ((:file "package")
               (:cffi-grovel-file "grovel")
               (:file "terminal")))

(defsystem "cl-tui/tests"
  :depends-on (:cl-tui :fiveam)
  :pathname "tests"
  :serial t
  :components ((:file "main"))
  :perform (test-op (op c)
                    (unless (symbol-call '#:fiveam '#:run! :cl-tui/tests)
                      (error "Tests failed"))))
