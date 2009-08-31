(eval-when-compile (require 'cl))

(defun rbdbg-directory ()
  "The directory of this file, or nil."
  (let ((file-name (or load-file-name
                       (symbol-file 'rbdbg-directory)
		       (buffer-file-name (current-buffer))
		       )))
    (if file-name
        (expand-file-name (file-name-directory file-name))
      nil)))

(defun rbdbg-require-relative (filename &optional source-first)
  "load FILE relative to  `rbdbg-directory'"
  (lexical-let* ((ext (file-name-extension filename))
		 (file-name-short 
		  (expand-file-name (concat (rbdbg-directory) filename)))
		 (first-filename file-name-short)
		 (el  (concat file-name-short ".el"))
		 (elc (concat file-name-short ".elc"))
		 file-list ret-value)
    (cond ((equal ext "el")
	   (setq first-filename file-name-short)
	   (setq el nil))
	  ((equal ext "elc")
	   (setq first-filename file-name-short)
	   (setq elc nil)))
    
    (if source-first 
	(setq file-list (list first-filename el elc))
      (setq file-list (list first-filename elc el)))
    (loop for filename in file-list do
	  (if (and filename (file-readable-p filename))
	      (progn 
		(load filename)
		(setq ret-value t)
		(return))
	    ))
    ret-value))

  
(defun rbdbg-load-all-files()
  (lexical-let ((file-list '("rbdbgr-regexp" "rbdbg-var" 
			     "rbdbg-loc" "rbdbg-lochist" "rbdbg-file"
			     "rbdbg-window" "rbdbg-track" "rbdbg-track-mode"))
		(filename)
		(full-filename))
    (setq load-path (cons (rbdbg-directory) load-path))
    (loop for filename in file-list do
	  ;; We always want the source for now
	  (setq full-filename (format "%s%s.el" (rbdbg-directory) filename))
	  (message "Rocky is loading %s" full-filename)
	  (load-file full-filename))
    (setq load-path (cdr load-path))
    ))
  
