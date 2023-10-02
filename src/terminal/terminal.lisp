(in-package :cl-tui/terminal)
;; (in-readtable :interpol-syntax)

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
