(load-file "./behave.el")
(load-file "../rbdbgr-regexp.el")

(behave-clear-contexts)

(defun loc-match(text) (string-match rbdbgr-loc-regexp text))

(context "location matching: "
	 (tag regexp)
	 (lexical-let ((text ".. (./rbdbgr.rb:73)"))
	   (specify "basic location"
		    (expect (numberp (loc-match text)) t))
	   (specify "extract file name"
		    (message "%s" (match-string rbdbgr-loc-regexp-file-group text))
		    (expect (match-string rbdbgr-loc-regexp-file-group text)
			    equal "./rbdbgr.rb"))
	   (specify "extract line number"
		    (expect (match-string rbdbgr-loc-regexp-line-group text)
			    equal "73"))
	   
	   (specify "unmatched location"
		    (setq text "--> #0 METHOD Object#square(x) in file ./rbdbgr.rb at line 73")
		    (expect (numberp (loc-match text)) equal nil))
	   
	   ))

(behave "regexp")

