(defpackage :cl-tui/tests
  (:use #:cl #:fiveam))

(in-package :cl-tui/tests)

(def-suite :cl-tui :description "Tests for cl-tui")
(def-suite* :cl-tui/tests :in :cl-tui)

(test ok
  (is (= 1 1)))
