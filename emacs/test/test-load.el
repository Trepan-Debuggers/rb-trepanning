(load-file "./behave.el")
(load-file "../rbdbg-load.el")

(defun setup()
  (setq loc-current (symbol-file 'rbdbg-loc-current))
  (if loc-current 
      (progn 
	(fmakunbound 'rbdbg-loc-current)
	(setq load-history '())
	(load-file "../rbdbg-load.el")))
  (setq rbdbg-loc-file (concat (rbdbg-directory) "rbdbg-loc.el")))

(behave-clear-contexts)

(context "rdbg-load: "
	 (tag load)
	 (specify "Initialized history"
		  (setup)
		  (expect (symbol-file 'rbdbg-loc-current) equal nil))
	 (specify "Read el file when none exists"
		  (rbdbg-require-relative "rbdbg-loc.el" t)
		  (expect (symbol-file 'rbdbg-loc-current) equal
			  rbdbg-loc-file))
	 (setup)
	 ;; Try byte compiling a file and then rereading
	 (setq rbdbg-loc-file-compiled (concat rbdbg-loc-file "c"))
	 (unless (file-readable-p rbdbg-loc-file-compiled)
	   (byte-compile-file rbdbg-loc-file))
	 (if (file-readable-p rbdbg-loc-file-compiled)
	     (progn 
	       (rbdbg-require-relative "rbdbg-loc.elc" nil)
	       (specify "Read elc file when we already have read in an el file"
			(rbdbg-require-relative (file-name-nondirectory
						 rbdbg-loc-file-compiled))
			(expect (symbol-file 'rbdbg-loc-current) equal
				rbdbg-loc-file-compiled))
	       ;; (setup)
	       ;; (specify "Read el file by default when we already
	       ;; have read in an elc file"
	       ;; 		(rbdbg-require-relative "rdbg-loc" t)
	       ;; 		(expect (symbol-file 'rbdbg-loc-current) equal
	       ;; 			rbdbg-loc-file))
	       )))

(behave "load")

