(in-package :cl-tui/terminal)
(named-readtables:in-readtable :interpol-syntax)

(defparameter *term-output* *standard-output* "The terminal output stream.")

(defun term-raw ()
  "Place the terminal into 'raw' mode, no echo or delete.
This allows characters to be read directly without waiting for a newline.
See 'man 3 termios' for more information."
  #+win32 (error "`term-raw' not implemented for windows.")
  #-sbcl (error "`term-raw' not implemented for non-SBCL.")
  #+sbcl
  (let ((options (sb-posix:tcgetattr 0)))
    (setf (sb-posix:termios-lflag options)
          (logand (sb-posix:termios-lflag options)
                  (lognot (logior sb-posix:icanon
                                  sb-posix:echo
                                  sb-posix:echoe
                                  sb-posix:echok
                                  sb-posix:echonl))))
    (sb-posix:tcsetattr 0 sb-posix:TCSANOW options)))

(defun term-unraw ()
  "Place the terminal out of 'raw' mode, with echo and delete.
This allows characters to be read directly without waiting for a newline.
See 'man 3 termios' for more information."
  #+win32 (error "`term-unraw' not implemented for windows.")
  #-sbcl (error "`term-unraw' not implemented for non-SBCL.")
  #+sbcl
  (let ((options (sb-posix:tcgetattr 0)))
    (setf (sb-posix:termios-lflag options)
          (logior (sb-posix:termios-lflag options)
                  sb-posix:icanon
                  sb-posix:echo
                  sb-posix:echoe
                  sb-posix:echok
                  sb-posix:echonl))
    (sb-posix:tcsetattr 0 sb-posix:TCSANOW options)))

(defmacro with-raw-term (&body body)
  "Execute BODY with the terminal in raw mode."
  `(progn
     (term-raw)
     (unwind-protect
          (progn ,@body)
       (term-unraw))))

;; retrieve terminal size
(define-foreign-type ioctl-result-type ()
  ()
  (:actual-type :int)
  (:simple-parser ioctl-result))

(define-condition ioctl (error)
  ((ret :initarg :ret :initform nil :reader ret))
  (:report (lambda (condition stream)
             (format stream "IOCTL call failed with return value ~d~%~
(NOTE: IOCTL fails when called from slime.)"
                     (ret condition)))))


(defcfun ("ioctl" %ioctl) ioctl-result
  (fd :int)
  (request :unsigned-long)
  (winsz (:pointer (:struct winsize))))

(defun term-size ()
  "Return terminal size information.
The following are returned in a property list row, col, xpixels,
ypixels.  See `man 2 ioctl` for more inforamtion.  Note ioctl and thus
`term-size' will throw an error of type IOCTL when called from SLIME."
  (restart-case
      (with-foreign-object (wnsz '(:struct winsize))
        (%ioctl STDOUT-FILENO TIOCGWINSZ wnsz)
        (with-foreign-slots ((row col xpixel ypixel) wnsz (:struct winsize))
          `(:row ,row
                 :col ,col
                 :xpixel ,xpixel
                 :ypixel ,ypixel)))
    (return-no-extent-term ()
      :report "Return info for a terminal with no extent."
      '(:row 0
        :col 0
        :xpixel 0
        :ypixel 0))))

;; screen handling and escape sequences
;; TODO: these should be constants really
(defvar +term-clear+ #?"\x1b[H[2J" "clear the scren")
(defvar +term-alternate-screen+ #?"\x1b[?1049h" "enter alternate screen")
(defvar +term-normal-screen+ #?"\x1b[?1049l" "leave alternate screen")

(defun enter-alternate-screen ()
  (write-sequence +term-alternate-screen+ *term-output*)
  (write-sequence +term-clear+ *term-output*)
  (finish-output *term-output*))

(defun leave-alternate-screen ()
  (write-sequence +term-normal-screen+ *term-output*)
  (finish-output *term-output*))

(defmacro with-alternate-screen (&body body)
  `(progn
     (enter-alternate-screen)
     (unwind-protect
          (progn ,@body)
       (leave-alternate-screen))))
