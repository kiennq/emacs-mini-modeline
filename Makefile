.PHONY : test

EMACS ?= emacs

LOADPATH = -L . -L ./test

test:
	$(EMACS) -Q -batch $(LOADPATH) \
		-l test/test-utils.el \
		-f ert-run-tests-batch-and-exit
