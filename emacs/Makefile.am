lisp_files = rbdbg-loc.el \
	     rbdbg-locring.el

check_DATA = test/behave.el	\
	test/test-loc.el  	\
	test/test-locring.el

EXTRA_DIST = $(lisp_files) $(check_DATA)
ELCFILES   = rbdbg-loc.elc \
	     rbdbg-locring.elc


if INSTALL_EMACS_LISP
lisp_LISP = $(lisp_files)
CHECK_FILES = \
	check-loc \
	check-locring

check: $(CHECK_FILES)

check-loc: rbdbg-loc.el
	(cd $(srcdir)/test && \
	$(EMACS) -batch -Q -l test-loc.el )

check-locring: rbdbg-locring.el
	(cd $(srcdir)/test && \
	$(EMACS) -batch -Q -l test-locring.el )

endif

PHONY = $(CHECK_FILES)

test: check




