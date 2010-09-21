# I'll admit it -- I'm an absent-minded old-timer who has trouble
# learning new tricks.
.PHONY: check test all

test: check
all: check
	true

%: 
	rake $@
