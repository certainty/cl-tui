(defpackage :cl-tui/test
  (:use :cl :rove)
  (:nicknames :cl-tui-test)
  (:export #:test-suite))

(in-package :cl-tui/test)

(deftest myfirst
   (ok (eq 1 1)))
