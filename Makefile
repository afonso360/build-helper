EMACS ?= emacs
CASK ?= cask

all: test

test: clean-elc
	${MAKE} compile
	${MAKE} unit
	${MAKE} clean-elc

unit:
	${CASK} exec $(EMACS) -Q -batch -L . \
		-l test/build-helper-test.el \
		-f ert-run-tests-batch-and-exit

compile:
	${CASK} exec ${EMACS} -Q -batch -f batch-byte-compile build-helper.el

clean-elc:
	rm -f build-helper.elc

.PHONY: all test unit
