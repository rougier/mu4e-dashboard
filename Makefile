.POSIX:

EMACS = emacs
EVAL := $(EMACS) --eval
MU4E = /opt/homebrew/share/emacs/site-lisp/mu/mu4e
ELISP = mu4e-dashboard.el

PKGDIR := .

# Additional emacs loadpath
LOADPATH	:= -L $(PKGDIR)

# Prefer emacs config folder in XDG_CONFIG_HOME to ~/.emacs.d
# Assume emacs-user-directory is ~/.emacs.d
# Try to find ELPA directory or STRAIGHT directory.
XDG_ELPA_DIR	:= $(if $(XDG_CONFIG_HOME), $(XDG_CONFIG_HOME)/emacs/elpa, $(HOME)/.config/emacs/elpa)
ELPA_DIR := $(if $(shell test -d $(XDG_ELPA_DIR)), $(XDG_ELPA_DIR), $(HOME)/.emacs.d/elpa)

XDG_STRAIGHT_DIR := $(if $(XDG_CONFIG_HOME), $(XDG_CONFIG_HOME)/emacs/straight/build, $(HOME)/.config/emacs/straight/build)
STRAIGHT_DIR := $(if $(shell test -d $(XDG_STRAIGHT_DIR)), $(XDG_STRAIGHT_DIR), $(HOME)/.emacs.d/straight/build)

ASYNC_ELPA_DIR  =  $(shell \
	test -d $(ELPA_DIR) && \
	find -L $(ELPA_DIR) -maxdepth 1 -regex '.*/async-[.0-9]*' 2> /dev/null | \
	sort | tail -n 1)
ifneq "$(ASYNC_ELPA_DIR)" ""
	LOADPATH += -L $(ASYNC_ELPA_DIR)
endif

ASYNC_STRAIGHT_DIR  =  $(shell \
	test -d $(STRAIGHT_DIR) && \
	find -L $(STRAIGHT_DIR) -maxdepth 1 -regex '.*/async' 2> /dev/null | \
	sort | tail -n 1)
ifneq "$(ASYNC_STRAIGHT_DIR)" ""
	LOADPATH += -L $(ASYNC_STRAIGHT_DIR)
endif

all: lints $(ELISP:.el=.elc)

lints: checkdoc

checkdoc:
	for f in $(ELISP); do $(EMACS) -batch -Q --eval "(checkdoc-file \"$$f\")"; done

check: all
	srcdir=$(PWD) $(EMACS) -batch -Q $(LOADPATH) -L $(MU4E) -l test/regression-tests.el -f ert-run-tests-batch

clean:
	rm -f $(ELISP:.el=.elc)

.SUFFIXES: .el .elc

.el.elc:
	$(EMACS) -batch -Q $(LOADPATH) -L . -L $(MU4E) -f batch-byte-compile $<
