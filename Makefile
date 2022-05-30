.PHONY: test

test:
	emacs -Q -batch \
		-l ert \
		-l cern-root-mode.el \
		-l tests/test-cern-root-mode.el \
		-f ert-run-tests-batch-and-exit

