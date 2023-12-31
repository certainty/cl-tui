(include "sys/ioctl.h" "stdio.h" "unistd.h")

(in-package :cl-tui/terminal)

(constant (STDOUT-FILENO "STDOUT_FILENO"))
(constant (TIOCGWINSZ "TIOCGWINSZ"))
(cstruct winsize "struct winsize"
         (row "ws_row" :type :unsigned-short)
         (col "ws_col" :type :unsigned-short)
         (xpixel "ws_xpixel" :type :unsigned-short)
         (ypixel "ws_ypixel" :type :unsigned-short))
