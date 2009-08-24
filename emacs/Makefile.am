lisp_files = rbdbg-loc.el \
	     rbdbg-lochist.el

check_DATA = test/behave.el	\
	test/test-loc.el  	\
	test/test-lochist.el

EXTRA_DIST = $(lisp_files) $(check_DATA)
ELCFILES   = rbdbg-loc.elc \
	     rbdbg-lochist.elc


if INSTALL_EMACS_LISP
lisp_LISP = $(lisp_files)
CHECK_FILES = \
	check-loc \
	check-lochist

check: $(CHECK_FILES)

check-loc: rbdbg-loc.el
	(cd $(srcdir)/test && \
	$(EMACS) -batch -Q -l test-loc.el )

check-lochist: rbdbg-lochist.el
	(cd $(srcdir)/test && \
	$(EMACS) -batch -Q -l test-lochist.el )

endif

if MAINTAINER_MODE

CL = ChangeLog
ChangeLog:
	git log --pretty --numstat --summary | $(GIT2CL) > $@

ACLOCAL_AMFLAGS=-I .

endif

PHONY = $(CHECK_FILES) $(CL)

test: check




