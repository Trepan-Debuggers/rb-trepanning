# I'll admit it -- I'm an absent-minded old-timer who has trouble
# learning new tricks.
.PHONY: all test

all: test

#: Run all tests without bloated output
check-short: 
	$(MAKE) check 2>&1  | ruby check-filter.rb

#: Run all tests (same as "test")
check: 
	rake test

#: Run all tests (same as "check")
test: 
	rake test

%: 
	rake $@
