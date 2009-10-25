(load-file "./behave.el")
(load-file "../rbdbgr-core.el")

(behave-clear-contexts)

(context "command argument processing: "
	 (tag cmd-args)
	 (lexical-let ((opt-two-args '("0" "C" "e" "E" "F" "i")))
	   (specify "Two args found, none remain afterwards though."
		    (expect 
		     (rbdbgr-strip-command-arg '("-0" "a") '() opt-two-args)
		     equal nil))
	   (specify "Two args not found, strip first arg though."
		    (expect 
		     (rbdbgr-strip-command-arg '("-5" "a" "-0") '() opt-two-args)
		     equal '("a" "-0")))
	   (specify "Degenerate case - no args"
		    (expect 
		     (rbdbgr-strip-command-arg '() '() opt-two-args)
		     equal nil))
	   ))

(behave "cmd-args")

