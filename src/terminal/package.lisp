(defpackage :cl-tui/terminal
  (:use :cl :cffi :cl-interpol)
  (:nicknames :tuiterm)
  (:export
   :term-raw
   :term-unraw
   :with-raw-term
   :term-size
   :ioctl
   :+term-clear+
   :with-alternate-screen
   :*term-output*))
