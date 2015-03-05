Command examples
================

	# This line does nothing. It is a comment. Useful in debugger command files.
	    # This line also does nothing.
	s    # by default, this is an alias for the "step" command
	!s   # shows the value of variable "s".
	!!s  # Evaluates "!s" (or "not s"). The first ! is indicates evaluate.
	 !s   # Same as above, since there is a space in column one.

	info program;; list # Runs two commands "info program" and "list"
	pr  "hi ;;-)"  # Syntax error since ;; splits the line and " is not closed.
	!puts "hi ;;-)" # One way to do the above.

See also:
---------

`macro`, `alias`, `irb`, `set auto eval`, `set auto irb`, `set
abbrev`, `info macro`, and the *show* variants of the above *set*
commands.
