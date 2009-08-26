(load-file "./behave.el")
(load-file "../rbdbg-loc.el")

(behave-clear-contexts)

(lexical-let ((saved-buffer (current-buffer)))
  ; Below, we need to make sure current-buffer has an associated
  ; file with it.
  (find-file (symbol-file 'behave))

  (context "location field extraction: "
	   (tag loc)
	   (lexical-let* ((filename (buffer-file-name (current-buffer)))
			  (mark (point-marker))
			  (good-loc (make-rbdbg-loc 
				     :filename filename 
				     :line-number 5 
				     :mark mark))
			  (good-loc2 (make-rbdbg-loc 
				      :filename filename 
				      :line-number 6))
			  (good-loc3 (rbdbg-loc-current)))
	     
	     (specify "line-number extraction"
		      (expect (rbdbg-loc-line-number good-loc) equal 5))
	     (specify "mark extraction"
		      (expect (rbdbg-loc-mark good-loc) equal mark))

	     (specify "mark set"
		      (rbdbg-loc-mark= good-loc2 mark)
		      (expect (rbdbg-loc-mark good-loc2) equal mark))

	     ))
  (switch-to-buffer saved-buffer))
(behave "loc")

; TODO: add test for debug-loc-goto, e.g.
;(rbdbg-loc-goto (rbdbg-loc-new "/tmp/bashdb.diff" 8))
;(rbdbg-loc-goto (rbdbg-loc-new "/tmp/bashdb.diff" 8) 'other-window 1)
