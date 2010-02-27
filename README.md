A modular, testable debugger for Ruby 1.9

Some cryptic examples.

Running if rbdbgr is installed:

bc.  rbdbgr ruby-program [program]

If your program needs options of its own:

bc.  rbdbgr -- ruby-program [program args...]

Running from inside irb:

bc.  require 'rbdbgr'
 Debugger.debug { your code }

You can do the same thing inside your Ruby program, but probably you don't want to give a block. Instead, you may want to have debugging start on the next statement in the code: 

bc.  require 'rbdbgr'
 Debugger.debug # Don't stop here...  
 work           # but stop here.

or if you haven't mucked around with _$0_ and _ARGV_, you might try:

bc.  Debugger.debug(:set_restart=>true)

which informs the debugger on how to restart the program (via the _restart_ command) using the values of _ARGV_ and _$0_ at the time _Debugger.debug_ was called.

If you want a synchronous stop in your program call to the
debugger at the point of the call, set opts[:immediate]
true. Example:

The above is really shorthand for something like:

bc.  mydbg = Debugger.new(:set_restart=>true)
 mydbg.debugger

_Debugger.debug_ returns a debugger object (_mydbg_ above) which can then be used over again. The debugger object holds debugger settings.

If for some reason you want a synchronous stop in your program call to the debugger at the point of the call, set _opts[:immediate]_ to _true_. Example:


bc.  #    ... work, work, work
 mydbg.debugger(:immediate=>true)   # enter debugger here
 #    ... work, work, work
