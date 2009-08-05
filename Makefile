# I'll admit it, I'm an absent-minded old-timer who has trouble learning new tricks.
.PHONY: check test install package

PROGRAM='rbdbgr'

check: test
test: 
	rake test

# package: 
# 	rake package
#
# install: package
# 	rake package
# 	(cd pkg && sudo gem install $(PROGRAM)*.gem)
