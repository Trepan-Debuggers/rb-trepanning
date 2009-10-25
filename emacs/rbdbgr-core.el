;; -------------------------------------------------------------------
;; Dependencies.
;;
(eval-when-compile
  (require 'cl))
(setq load-path (cons nil (cons "." load-path)))
(require 'rbdbg-track)
(require 'gud)  ; FIXME: GUD is BAD! It is too far broken to be fixed.
(require 'rbdbgr-regexp)
(setq load-path (cddr load-path))


;; FIXME: move to a more common location. rbdbg-core ? 
(defun rbdbgr-strip-command-arg (args two-args opt-two-args)
  "return ARGS with the first argument, an 'option'
removed. 

However if that option argument may take another argument, remove
that as well. TWO-ARGS is list of options (strings without the
leading dash) that take a mandatory additional
argument. OPT-TWO-ARGS is a list of options might take two
arguments. The rule for an optional argument we use: if the next
parameter starts with a dash ('-'), it is not part of the
preceeding parameter when that parameter is optional.

NOTE: we don't check whether the first arguments of ARGS is an
option by testing to see if it starts say with a dash. So on
return the first argument is always removed.
"
  (let ((arg (car args))
	(d-two-args (mapcar (lambda(x) (concat "-" x)) two-args))
	(d-opt-two-args (mapcar (lambda(x) (concat "-" x)) opt-two-args))
	(remaining (cdr args)))
    (cond 
     ((member arg d-two-args)
      (if remaining (cdr remaining)
	  (progn 
	    (message "Expecting an argument after %s. Continuing anyway."
		     arg)
	    remaining)))
     ((member arg d-opt-two-args)
      (if (and remaining (not (string-match "^-" (car remaining))))
	  (cdr remaining)
	remaining))
     (t remaining))))

(defun rbdbgr-get-script-name (orig-args)
  "Parse command line ARGS for the annotate level and name of script to debug.

ARGS should contain a tokenized list of the command line to run.

We return the script name to be debugged and whether option the
annotate option (either '--annotate' or '-A') was set."
  ;; Parse the following:
  ;;
  ;;  [ruby ruby-options] rbdbgr rbdbgr-options script-name script-options
  (let ((args orig-args)
	(script-name nil)
	(annotate-p nil)
	(ruby-opt-two-args '("0" "C" "e" "E" "F" "i"))
	;; Ruby doesn't have mandatory 2-arg options in our sense,
	;; since the two args can be run together, e.g. "-C/tmp" or "-C /tmp"
	;; 
	(ruby-two-args '())
	(debugger-name)
	;; One dash is added automatically to the below, so
	;; h is really -h and -host is really --host.
	(rbdbgr-two-args '("h" "-host" "p" "-port"
			   "I" "-include" "-r" "-require"))
	(rbdbgr-opt-two-args '()))
    (if (not (and args))
	;; Got nothing: return '(nil, nil)
	'(script-name annotate-p)
      ;; else
      ;; Strip off optional "ruby" or "ruby182" etc.
      (when (string-match "^ruby[-0-9]*$"
			  (file-name-sans-extension
			   (file-name-nondirectory (car args))))
	(pop args) ; remove whatever "ruby" thing found.

	;; Strip off Ruby-specific options
	(while (and args
		    (string-match "^-" (car args)))
	  (setq args (rbdbgr-strip-command-arg 
		      args ruby-two-args ruby-opt-two-args))))

      ;; Remove "rbdbgr" from "rbdbgr --rbdbgr-options script
      ;; --script-options"
      (setq debugger-name (file-name-sans-extension
			   (file-name-nondirectory (car args))))
      (unless (string-match "^rbdbgr$" debugger-name)
	(message 
	 "Expecting debugger name to be rbdbgr; got `%s'. Stripping anyway."
	 debugger-name))
      (pop args)
      ;; Skip to the first non-option argument.
      (while (and args (not script-name))
	(let ((arg (pop args)))
	  (cond
	   ;; Annotation or emacs option with level number.
	   ((or (member arg '("--annotate" "-A"))
		(equal arg "--emacs"))
	    (setq annotate-p t)
	    (pop args))
	   ;; Combined annotation and level option.
	   ((string-match "^--annotate=[0-9]" arg)
	    (setq annotate-p t))
	   ;; Options with arguments.
	   ((string-match "^-" arg)
	    (setq args (rbdbgr-strip-command-arg 
			args rbdbgr-two-args rbdbgr-opt-two-args)))
	   ;; Anything else must be the script to debug.
	   (t (setq script-name arg)))))
      (list script-name annotate-p))))

(defun rbdbgr-goto-line-for-type (type pt)
  "Display the location mentioned in line described by PT. TYPE is used
to get a regular-expresion pattern matching information."
  (interactive "d")
  (save-excursion
    (goto-char pt)
    (lexical-let* ((proc-buff (current-buffer))
		   (proc-window (selected-window))
		   (curr-proc (get-buffer-process proc-buff))
		   (start (line-beginning-position))
		   (end (line-end-position))
		   (tb (gethash type rbdbgr-dbgr-pat-hash))
		   ;; FIXME check that tb is not null and abort if it is.
		   (loc (rbdbg-track-loc (buffer-substring start end)
					 (rbdbg-dbgr-loc-pat-regexp tb)
					 (rbdbg-dbgr-loc-pat-file-group tb)
					 (rbdbg-dbgr-loc-pat-line-group tb)
					 )))
    (if loc (rbdbg-track-loc-action loc proc-buff proc-window)))))

(defun rbdbgr-goto-traceback-line (pt)
  "Display the location mentioned by the Ruby traceback line
described by PT."
  (interactive "d")
  (rbdbgr-goto-line-for-type "traceback" pt))

(defun rbdbgr-goto-dollarbang-traceback-line (pt)
  "Display the location mentioned by the Ruby $! traceback line
described by PT."
  (interactive "d")
  (rbdbgr-goto-line-for-type "dollar-bang" pt))

;; Perform initializations common to all debuggers.
;; The first arg is the specified command line,
;; which starts with the program to debug.
;; The other three args specify the values to use
;; for local variables in the debugger buffer.
(defun rbdbgr-common-init (rbdbgr-buffer-name rbdbgr-cmd-buffer target-name
					      program args
					      marker-filter
					      &optional find-file)
  "Perform initializations common to all debuggers.

RBDBGR-BUFFER-NAME is the specified command line, which starts
with the program to debug. PROGRAM, ARGS and MARKER-FILTER
specify the values to use for local variables in the debugger
buffer."
  (if rbdbgr-cmd-buffer
      (progn
	(pop-to-buffer rbdbgr-cmd-buffer)
	(when (and rbdbgr-cmd-buffer (get-buffer-process rbdbgr-cmd-buffer))
	  (error "This program is already being debugged"))
	(apply 'make-comint rbdbgr-buffer-name program nil args)
	(or (bolp) (newline)))
    (pop-to-buffer (setq rbdbgr-cmd-buffer
			 (apply 'make-comint rbdbgr-buffer-name program nil
				args))))
  
  ;; Since comint clobbered the mode, we don't set it until now.
  (gud-mode)
  (set (make-local-variable 'gud-target-name) target-name)
  (set (make-local-variable 'gud-marker-filter) marker-filter)
  (set (make-local-variable 'gud-minor-mode) 'rbdbgr)
  (set (make-local-variable 'gud-last-frame) nil)
  (set (make-local-variable 'gud-last-last-frame) nil)

  (let ((buffer-process (get-buffer-process (current-buffer))))
    (if buffer-process
	(progn 
	  (set-process-filter buffer-process 'gud-filter)
	  (set-process-sentinel buffer-process 'gud-sentinel))))
  (gud-set-buffer))

;; ;;;###autoload
;; (defun rbdbgr (command-line)
;;   "Invoke the rbdbgr Ruby debugger and start the Emacs user interface.

;; String COMMAND-LINE specifies how to run rbdbgr."
;;   (interactive
;;    (let ((init (buffer-file-name)))
;;      (setq init (and init
;;                      (file-name-nondirectory init)))
;;      (list (gud-query-cmdline 'rbdbgr init))))
;;   ;; (rbdbgr-set-window-configuration-state 'debugger t)
;;   ;; Parse the command line and pick out the script name and whether
;;   ;; --annotate has been set.
;;   (let* ((words (with-no-warnings
;; 		  (split-string-and-unquote command-line)))
;; 	 (script-name-annotate-p (rbdbgr-get-script-name
;; 				  (gud-rbdbgr-massage-args "1" words)))
;; 	 (target-name (file-name-nondirectory (car script-name-annotate-p)))
;; 	 (annotate-p (cadr script-name-annotate-p))
;; 	 (cmd-buffer-name (format "rbdbgr-cmd-%s" target-name))
;; 	 (rbdbgr-cmd-buffer-name (format "*%s*" cmd-buffer-name))
;; 	 (rbdbgr-cmd-buffer (get-buffer rbdbgr-cmd-buffer-name))
;; 	 (program (car words))
;; 	 (args (cdr words))
;; 	 (gud-chdir-before-run nil))
    
;;     ;; `gud-rbdbgr-massage-args' needs whole `command-line'.
;;     ;; command-line is refered through dynamic scope.
;;     (rbdbgr-common-init cmd-buffer-name rbdbgr-cmd-buffer target-name
;; 			program args
;; 			'gud-rbdbgr-marker-filter
;; 			'gud-rbdbgr-find-file)
;;     (setq comint-process-echoes t)
    
;;     ;; (setq rbdbgr-inferior-status "running")
    
;;     (rbdbgr-command-initialization)
    
;;     ;; Setup exit callback so that the original frame configuration
;;     ;; can be restored.
;;     ;; (let ((process (get-buffer-process rbdbg-buffer)))
;;     ;;   (when process
;;     ;;     (unless (equal rbdbgr-line-width 120)
;;     ;; 	    (gud-call (format "set width %d" rbdbgr-line-width)))
;;     ;;     (set-process-sentinel process
;;     ;;                           'rbdbgr-process-sentinel)))
    
    
;;     ;; Add the buffer-displaying commands to the Gud buffer,
;;     ;; FIXME: combine with code in rbdbgr-track.el; make common
;;     ;; command buffer mode map.
;;     (let ((prefix-map (make-sparse-keymap)))
;;       (define-key (current-local-map) gud-key-prefix prefix-map)
;;       (define-key prefix-map "t" 'rbdbgr-goto-traceback-line)
;;       (define-key prefix-map "!" 'rbdbgr-goto-dollarbang-traceback-line)
;;       (rbdbgr-populate-secondary-buffer-map-plain prefix-map))
    
;;     (rbdbgr-populate-common-keys (current-local-map))
;;     (rbdbgr-populate-debugger-menu (current-local-map))
    
;;     ;; (setq comint-prompt-regexp (concat "^" rbdbgr-input-prompt-regexp))
;;     ;; (setq paragraph-start comint-prompt-regexp)
    
;;     ;; (setcdr (assq 'rbdbgr-debugger-support-minor-mode minor-mode-map-alist)
;;     ;;         rbdbgr-debugger-support-minor-mode-map-when-active)
;;     ;; (when rbdbgr-many-windows
;;     ;;   (rbdbgr-setup-windows-initially))
    
;;     (run-hooks 'rbdbgr-mode-hook)))


(defun rbdbgr-reset ()
  "Rbdbgr cleanup - remove debugger's internal buffers (frame,
breakpoints, etc.)."
  (interactive)
  ;; (rbdbgr-breakpoint-remove-all-icons)
  (dolist (buffer (buffer-list))
    (when (string-match "\\*rbdbgr-[a-z]+\\*" (buffer-name buffer))
      (let ((w (get-buffer-window buffer)))
        (when w
          (delete-window w)))
      (kill-buffer buffer))))

;; (defun rbdbgr-reset-keymaps()
;;   "This unbinds the special debugger keys of the source buffers."
;;   (interactive)
;;   (setcdr (assq 'rbdbgr-debugger-support-minor-mode minor-mode-map-alist)
;; 	  rbdbgr-debugger-support-minor-mode-map-when-deactive))


(defun rbdbgr-customize ()
  "Use `customize' to edit the settings of the `rbdbgr' debugger."
  (interactive)
  (customize-group 'rbdbgr))


;; -------------------------------------------------------------------
;; The end.
;;

(provide 'rbdbgr-core)

;;; Local variables:
;;; eval:(put 'rbdbgr-debug-enter 'lisp-indent-hook 1)
;;; End:

;;; rbdbgr-core.el ends here
