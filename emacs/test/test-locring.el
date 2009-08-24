(load-file "./behave.el")
(load-file "../rbdbg-loc.el")
(load-file "../rbdbg-locring.el")

(behave-clear-contexts)

(lexical-let* ((saved-buffer (current-buffer)))
  ; Below, we need to make sure current-buffer has an associated
  ; file with it.
  (find-file (symbol-file 'behave))

  (context "location ring initialization and fields access: "
	   (tag locring)
	   (lexical-let ((loc-hist (rbdbg-location-history-new))
			 (filename (buffer-file-name (current-buffer)))
			 )
	     
	     (specify "get ring component for a new history ring"
		      (expect (ring-p (rbdbg-location-history-ring loc-hist))
			      equal t))
	     (specify "get ring index for a new history ring"
		      (expect (rbdbg-location-history-position loc-hist)
			      equal -1))
	     
	     (specify "get item for an empty history ring"
		      (expect (rbdbg-location-history-item loc-hist) equal nil))
	     
	     ))
  (switch-to-buffer saved-buffer))

(behave "locring")
