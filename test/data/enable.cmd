# ********************************************************
# This tests the enable command.
# ********************************************************
set basename off
break gcd
# Should have a breakpoint 1
enable 1
# An invalid enable command
enable foo
quit


