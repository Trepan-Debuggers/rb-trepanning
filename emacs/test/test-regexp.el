(load-file "./behave.el")
(load-file "../rbdbgr-regexp.el")

(behave-clear-contexts)

(defun loc-match(text) (string-match rbdbgr-position-regexp text))

(lexical-let* ((saved-buffer (current-buffer)))
  ; Below, we need to make sure current-buffer has an associated
  ; file with it.
  (find-file (symbol-file 'behave))

  (context "location matching: "
	   (tag regexp)
	   (lexical-let ((text ".. (./rbdbgr.rb:73)"))
	     (specify "basic location"
		      (expect (numberp (loc-match text)) t))
	     (specify "extract file name"
		      (message "%s" (match-string rbdbgr-marker-regexp-file-group text))
		      (expect (match-string rbdbgr-marker-regexp-file-group text)
			      equal "./rbdbgr.rb"))
	     (specify "extract line number"
		      (expect (match-string rbdbgr-marker-regexp-line-group text)
			      equal "73"))

	     (specify "unmatched location"
		      (setq text "--> #0 METHOD Object#square(x) in file ./rbdbgr.rb at line 73")
		      (expect (numberp (loc-match text)) equal nil))

	     ))
  
  (switch-to-buffer saved-buffer))
(behave "regexp")

