(load-file "./behave.el")
(load-file "../rbdbg-loc.el")

(behave-clear-contexts)

(lexical-let ((saved-buffer (current-buffer)))
  ; Below, we need to make sure current-buffer has an associated
  ; file with it.
  (find-file (symbol-file 'behave))

  (context "location creation: "
	   (tag loc)
	   (lexical-let* ((filename (buffer-file-name (current-buffer)))
			  (good-loc (rbdbg-loc-new filename 5 (point-marker)))
			  (good-loc2 (rbdbg-loc-current)))
	     
	     (specify "invalid location (two numbers)"
		      (expect (rbdbg-loc? (rbdbg-loc-new 5 5)) equal nil))
	     (specify "invalid location (bad marker)"
		      (expect (rbdbg-loc? (rbdbg-loc-new "f" 5 "f")) equal nil))
	     (specify "good location (via rbdbg-loc-current)"
	      	      (expect (rbdbg-loc? good-loc2) t))
	     (specify "good location (two args)"
		      (expect (rbdbg-loc? (rbdbg-loc-new filename 5)) t))
	     (specify "good location (three args)"
		      (expect (rbdbg-loc? good-loc) equal t))
	     ))
  
  (context "location field extraction: "
	   (tag loc)
	   (lexical-let* ((filename (buffer-file-name (current-buffer)))
			  (marker (point-marker))
			  (good-loc (rbdbg-loc-new filename 5 marker)))
	     
	     (specify "filename extraction"
		      (expect (rbdbg-loc-filename good-loc) equal filename))
	     (specify "line-number extraction"
		      (expect (rbdbg-loc-line-number good-loc) equal 5))
	     (specify "marker extraction"
		      (expect (rbdbg-loc-marker good-loc) equal marker))
	     
	     (specify "filename extraction - invalid location"
		      (expect (rbdbg-loc-filename nil) equal nil))
	     (specify "line-number extraction - invalid location"
		      (expect (rbdbg-loc-line-number nil) equal nil))
	     (specify "marker extraction - invalid location"
		      (expect (rbdbg-loc-marker nil) equal nil))
	     ))
  (switch-to-buffer saved-buffer))
(behave "loc")

