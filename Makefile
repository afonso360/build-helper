EMACS ?= emacs
CASK ?= cask

all: test

test: clean-elc
	${MAKE} unit
	${MAKE} compile
	${MAKE} unit
	${MAKE} clean-elc

unit:
	${CASK} exec ert-runner

compile:
	${CASK} exec ${EMACS} -Q -batch -f batch-byte-compile build-helper.el

clean-elc:
	rm -f build-helper.elc

.PHONY: all test docs unit
