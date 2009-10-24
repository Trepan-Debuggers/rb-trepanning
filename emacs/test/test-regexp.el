(load-file "./behave.el")
(load-file "../rbdbg-regexp.el")
(load-file "../rbdbg-var.el")

(behave-clear-contexts)


; Some setup usually done in setting up the buffer.
; We customize this for the debugger rbdbgr. Others may follow.
; FIXME: encapsulate this.
(setq dbg-name "rbdbgr")
(setq loc-pat (gethash dbg-name rbdbg-dbgr-pat-hash))

(setq rbdbg-dbgr (make-rbdbg-dbgr
		  :name dbg-name
		  :loc-regexp (rbdbg-dbgr-loc-pat-regexp loc-pat)
		  :file-group (rbdbg-dbgr-loc-pat-file-group  loc-pat)
		  :line-group (rbdbg-dbgr-loc-pat-line-group  loc-pat))) 


(defun loc-match(text) 
  (string-match (rbdbg-dbgr-loc-regexp rbdbg-dbgr) text)
)

(context "location matching: "
	 (tag regexp)
	 (lexical-let ((text ".. (./rbdbgr.rb:73)")
		       (text2 "C> ((eval):1 via /tmp/eval2.rb:2)")
		       (text3 "-- (<internal:prelude>:28 remapped prelude.rb:28)")
		       )
	   (specify "basic location"
		    (expect (numberp (loc-match text)) t))
	   (specify "extract file name"
		    (message (match-string (rbdbg-dbgr-file-group rbdbg-dbgr)
					   text)
	   	    (expect (match-string (rbdbg-dbgr-file-group rbdbg-dbgr)
	   				  text)
	   		    equal "./rbdbgr.rb")))
	   (specify "extract line number"
	   	    (expect (match-string (rbdbg-dbgr-line-group rbdbg-dbgr)
	   				  text)
	   		    equal "73"))

	   ;; Now try via
	   (specify "basic via location"
	   	    (expect (numberp (loc-match text2)) t))
	   (specify "extract via file name"
	   	    (expect (match-string (rbdbg-dbgr-file-group rbdbg-dbgr)
	   				  text2)
	   		    equal "/tmp/eval2.rb"))
	   (specify "extract via line number"
	   	    (expect (match-string (rbdbg-dbgr-line-group rbdbg-dbgr)
	   				  text2)
	   		    equal "2"))

	   ;; Now try remap
	   (specify "basic via location"
	   	    (expect (numberp (loc-match text3)) t))

	   ;;
	   (specify "unmatched location"
	   	    (setq text "--> #0 METHOD Object#square(x) in file ./rbdbgr.rb at line 73")
	   	    (expect (numberp (loc-match text)) equal nil))
	   
	   ))

(behave "regexp")

