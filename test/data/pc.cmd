# ********************************************************
# This tests the set register pc command.
# ********************************************************
info reg pc
set register pc 13
step
info reg pc
va
quit!
