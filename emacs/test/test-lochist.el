(load-file "./behave.el")
(load-file "../rbdbg-loc.el")
(load-file "../rbdbg-lochist.el")

;(behave-clear-contexts)

(lexical-let* ((saved-buffer (current-buffer)))
  ; Below, we need to make sure current-buffer has an associated
  ; file with it.
  (find-file (symbol-file 'behave))

  (context "location ring initialization and fields access: "
	   (tag lochist)
	   (lexical-let ((loc-hist (rbdbg-loc-hist-new))
			 (filename (buffer-file-name (current-buffer)))
			 (loc (rbdbg-loc-current)))
	     
	     (specify "get ring component for a new history ring"
		      (expect (ring-p (rbdbg-loc-hist-ring loc-hist))
			      equal t))
	     (specify "get ring index for a new history ring"
		      (expect (rbdbg-loc-hist-position loc-hist)
			      equal -1))
	     
	     (specify "get item for an empty history ring"
		      (expect (rbdbg-loc-hist-item loc-hist) equal nil))
	     
	     (specify "add an item to an empty history ring"
		      (rbdbg-loc-hist-add loc-hist loc)
		      (expect (rbdbg-loc-hist-item loc-hist) equal loc))

	     (specify "One item an empty history ring"
		      (expect (ring-length 
			       (rbdbg-loc-hist-ring loc-hist))
			      equal 1))
	     
	     (specify "duplicate item added is ignored"
		      (rbdbg-loc-hist-add loc-hist loc)
		      (expect (ring-length 
			       (rbdbg-loc-hist-ring loc-hist)) 
			      equal 1))
	     
	     ))
  (behave "lochist")
  (switch-to-buffer saved-buffer))
