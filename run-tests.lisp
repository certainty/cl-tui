(ql:quickload :cl-tui/test)
(in-package :cl-tui/test)
(uiop:quit (if (run-all-tests) 0 1))
