# I'll admit it -- I'm an absent-minded old-timer who has trouble
# learning new tricks.
.PHONY: all test

all: test
	rake $@

check: 
	rake test
test: 
	rake test

%: 
	rake $@
