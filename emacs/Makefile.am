lisp_files = rbdbg-loc.el \
	     rbdbg-lochist.el \
	     rbdbg-track.el \
	     rbdbgr-regexp.el

check_DATA = test/behave.el	\
	test/test-loc.el  	\
	test/test-lochist.el    \
	test/test-regexp.el

EXTRA_DIST = $(lisp_files) $(check_DATA)
ELCFILES   = rbdbg-loc.elc     \
	     rbdbg-track.elc   \
	     rbdbg-lochist.elc \
	     rbdbgr-regexp.elc


if INSTALL_EMACS_LISP
lisp_LISP = $(lisp_files)
CHECK_FILES = \
	check-loc \
	check-lochist \
	check-regexp

check: $(CHECK_FILES)

check-loc: rbdbg-loc.el
	(cd $(srcdir)/test && \
	$(EMACS) -batch -Q -l test-loc.el )

check-lochist: rbdbg-lochist.el
	(cd $(srcdir)/test && \
	$(EMACS) -batch -Q -l test-lochist.el )

check-regexp: rbdbg-lochist.el
	(cd $(srcdir)/test && \
	$(EMACS) -batch -Q -l test-regexp.el )

endif

if MAINTAINER_MODE

CL = ChangeLog
ChangeLog:
	git log --pretty --numstat --summary | $(GIT2CL) > $@

ACLOCAL_AMFLAGS=-I .

endif

PHONY = $(CHECK_FILES) $(CL)

test: check




