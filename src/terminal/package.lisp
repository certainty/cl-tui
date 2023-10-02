(defpackage :cl-tui/terminal
  (:use :cl :cffi :cl-interpol)
  (:export :term-raw :term-unraw :term-size :ioctl))
