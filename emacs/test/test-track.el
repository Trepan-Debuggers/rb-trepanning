(load-file "./behave.el")
(load-file "../rbdbgr-regexp.el")
(load-file "../rbdbg-loc.el")
(load-file "../rbdbg-var.el")
(load-file "../rbdbg-track.el")

(behave-clear-contexts)

; Some setup usually done in setting up the buffer.
; We customize this for the debugger rbdbgr. Others may follow.
; FIXME: encapsulate this.
(setq dbg-name "rbdbgr")
(setq loc-pat (gethash dbg-name rbdbg-dbgr-pat-hash))

(setq rbdbg-dbgr (make-rbdbg-dbgr
		  :name dbg-name
		  :loc-regexp (rbdbg-dbgr-loc-pat-regexp      loc-pat)
		  :file-group (rbdbg-dbgr-loc-pat-file-group  loc-pat)
		  :line-group (rbdbg-dbgr-loc-pat-line-group  loc-pat))) 

; FIXME/WARNING the below is customized for rbdbgr
(lexical-let* ((filename (symbol-file 'behave))
	       (debugger-output (format "-> (%s:7)\n(rbdbgr):\n" filename))
	       (loc (rbdbg-track-loc debugger-output)))

  (context "rdbg-file-line-count: "
	   (tag track)
	   (specify "loc extracted"
		    (expect (rbdbg-loc-p loc) t))
	   ))

(behave "track")

