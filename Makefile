.POSIX:

EMACS = emacs

MU4E = /usr/local/share/emacs/site-lisp/mu4e

ELISP = mu4e-dashboard.el

all: lints $(ELISP:.el=.elc)

lints: checkdoc

checkdoc:
	for f in $(ELISP); do $(EMACS) -batch -Q --eval "(checkdoc-file \"$$f\")"; done

check: all
	srcdir=$(PWD) $(EMACS) -batch -Q -L . -L $(MU4E) -l test/regression-tests.el -f ert-run-tests-batch

clean:
	rm -f $(ELISP:.el=.elc)

.SUFFIXES: .el .elc

.el.elc:
	$(EMACS) -batch -Q -L . -L $(MU4E) -f batch-byte-compile $<
