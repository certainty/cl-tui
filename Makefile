RUN_LISP=sbcl --noinform --non-interactive
LISP=sbcl --noinform

test:
	$(RUN_LISP) --non-interactive --eval '(asdf:test-system :cl-tui)'

repl:
	$(LISP)	--eval '(ql:quickload :cl-tui)'

build:
	$(RUN_LISP) --eval '(asdf:make :cl-tui)'

.PHONY: test repl
