(load-file "./behave.el")
(load-file "../rbdbg-loc.el")
(load-file "../rbdbg-file.el")

(behave-clear-contexts)

(lexical-let ((filename (symbol-file 'behave)))

  (context "rdbg-file-line-count: "
	   (tag file)
	   (specify "File not found"
		    (expect (rbdbg-file-line-count "not-found-file") equal nil))
	   (specify "File found"
		    (expect (integerp (rbdbg-file-line-count filename)) t))
	   )

  (context "rdbg-file-loc-from-line: "
	   (tag file)
	   (specify "File not found"
		    (expect (stringp (rbdbg-file-loc-from-line "not-found-file" 5)) t))
	   (specify "invalid real line number"
		    (expect (stringp (rbdbg-file-loc-from-line filename 5.5)) t))
	   (specify "negative number"
		    (expect (stringp (rbdbg-file-loc-from-line filename -1)) t))
	   (specify "Line number too large for file"
		    (expect (stringp (rbdbg-file-loc-from-line filename 10001)) t))
	   (specify "Line number too large for file"
		    (expect (rbdbg-loc? (rbdbg-file-loc-from-line filename 30)) t))
  ))
(behave "file")

