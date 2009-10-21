;; -------------------------------------------------------------------
;; Dependencies.
;;


(defun rbdbgr-get-script-name (args)
  "Parse command line ARGS.

A list containing the script name, and whether the annotate
option was set is returned.

Initially annotate should be set to nil.  Argument ARGS contains
a tokenized list of the command line."
  ;; Parse the following:
  ;;
  ;;  [ruby ruby-options] rdebug rdebug-options script-name script-options
  (and args
       (let ((name nil)
             (annotate-p nil))
         ;; Strip of optional "ruby" or "ruby182" etc.
         (when (string-match "^ruby[0-9]*$"
                             (file-name-sans-extension
                              (file-name-nondirectory (car args))))
           (pop args)
           (while (and args
                       (string-match "^-" (car args)))
             (if (member (car args) '("-e" "-r" "-I" "-C" "-F" "-K"))
                 (pop args))
             (pop args)))
         ;; Remove "rbdbgr" from "rbdbgr --rbdbgr-options script
         ;; --script-options"
         (pop args)
         ;; Skip to the first non-option argument.
         (while (and args
                     (not name))
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
              ((member arg '("-h" "--host" "-p" "--port"
                             "-I" "--include" "-r" "--require"))
               (pop args))
              ((string-match "^-" arg)
               nil)
              (t
               (setq name arg)))))
         (and name
              (list name annotate-p)))))

(defun rbdbgr-goto-traceback-line (pt)
  "Display the location PT in a source file of the Ruby traceback line."
  (interactive "d")
  (save-excursion
    (goto-char pt)
    (let ((s (buffer-substring (line-beginning-position) (line-end-position)))
	  (gud-comint-buffer (current-buffer)))
      (when (string-match rbdbgr-traceback-line-re s)
        (rbdbgr-display-line
         (substring s (match-beginning 1) (match-end 1))
         (string-to-number (substring s (match-beginning 2) (match-end 2))))
        ))))

(defun rbdbgr-goto-dollarbang-traceback-line (pt)
  "Display the location PT in a source file of the Ruby $! traceback line."
  (interactive "d")
  (save-excursion
    (goto-char pt)
    (let ((s (buffer-substring (line-beginning-position) (line-end-position)))
	  (gud-comint-buffer (current-buffer)))
      (when (string-match rbdbgr-dollarbang-traceback-line-re s)
        (rbdbgr-display-line
         (substring s (match-beginning 1) (match-end 1))
         (string-to-number (substring s (match-beginning 2) (match-end 2))))
        ))))


;; -------------------------------------------------------------------
;; The `rbdbgr' command and support functions.
;;

(defun rbdbgr-process-sentinel (process event)
  "Restore the original window configuration when the debugger process exits."
  (rbdbgr-debug-enter "rbdbgr-process-sentinel"
    (rbdbgr-debug-message "status=%S event=%S state=%S"
                          (process-status process)
                          event
                          rbdbgr-window-configuration-state)
    (gud-sentinel process event)
    ;; This will "flush" the last annotation. Especially "output"
    ;; (a.k.a. "starting") annotations don't have an end markers, if
    ;; the last command printed something.
    (if (string= event "finished\n")
        (gud-rbdbgr-marker-filter "\032\032\n"))
    ;; When the debugger process exited, when the comint buffer has no
    ;; buffer process (nil). When the debugger processes is replaced
    ;; with another process we should not restore the window
    ;; configuration.
    (when (and (or (eq rbdbgr-restore-original-window-configuration t)
                   (and (eq rbdbgr-restore-original-window-configuration :many)
                        rbdbgr-many-windows))
               (or (rbdbgr-dead-process-p)
                   (eq process (get-buffer-process gud-comint-buffer)))
               (eq rbdbgr-window-configuration-state 'debugger)
               (not (eq (process-status process) 'run)))
      (rbdbgr-internal-short-key-mode-off)
      (rbdbgr-set-window-configuration-state 'original)
      (rbdbgr-reset-keymaps))))


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

;;;###autoload
(defun rbdbgr (command-line)
  "Invoke the rbdbgr Ruby debugger and start the Emacs user interface.

String COMMAND-LINE specifies how to run rbdbgr."
  (interactive
   (let ((init (buffer-file-name)))
     (setq init (and init
                     (file-name-nondirectory init)))
     (list (gud-query-cmdline 'rbdbgr init))))
  (rbdbgr-debug-enter "rbdbgr"
    (rbdbgr-set-window-configuration-state 'debugger t)
    ;; Parse the command line and pick out the script name and whether
    ;; --annotate has been set.
    (let* ((words (with-no-warnings
                    (split-string-and-unquote command-line)))
           (script-name-annotate-p (rbdbgr-get-script-name
                                    (gud-rbdbgr-massage-args "1" words)))
           (target-name (file-name-nondirectory (car script-name-annotate-p)))
           (annotate-p (cadr script-name-annotate-p))
           (cmd-buffer-name (format "rbdbgr-cmd-%s" target-name))
           (rbdbgr-cmd-buffer-name (format "*%s*" cmd-buffer-name))
           (rbdbgr-cmd-buffer (get-buffer rbdbgr-cmd-buffer-name))
	   (program (car words))
	   (args (cdr words))
           (gud-chdir-before-run nil))

      ;; `gud-rbdbgr-massage-args' needs whole `command-line'.
      ;; command-line is refered through dynamic scope.
      (rbdbgr-common-init cmd-buffer-name rbdbgr-cmd-buffer target-name
			  program args
			  'gud-rbdbgr-marker-filter
			  'gud-rbdbgr-find-file)
      (setq comint-process-echoes t)

      (setq rbdbgr-inferior-status "running")

      (rbdbgr-command-initialization)

      ;; Setup exit callback so that the original frame configuration
      ;; can be restored.
      (let ((process (get-buffer-process gud-comint-buffer)))
        (when process
          (unless (equal rbdbgr-line-width 120)
	    (gud-call (format "set width %d" rbdbgr-line-width)))
          (set-process-sentinel process
                                'rbdbgr-process-sentinel)))


      ;; Add the buffer-displaying commands to the Gud buffer,
      ;; FIXME: combine with code in rbdbgr-track.el; make common
      ;; command buffer mode map.
      (let ((prefix-map (make-sparse-keymap)))
        (define-key (current-local-map) gud-key-prefix prefix-map)
	(define-key prefix-map "t" 'rbdbgr-goto-traceback-line)
	(define-key prefix-map "!" 'rbdbgr-goto-dollarbang-traceback-line)
        (rbdbgr-populate-secondary-buffer-map-plain prefix-map))

      (rbdbgr-populate-common-keys (current-local-map))
      (rbdbgr-populate-debugger-menu (current-local-map))

      (setq comint-prompt-regexp (concat "^" rbdbgr-input-prompt-regexp))
      (setq paragraph-start comint-prompt-regexp)

      (setcdr (assq 'rbdbgr-debugger-support-minor-mode minor-mode-map-alist)
              rbdbgr-debugger-support-minor-mode-map-when-active)
      (when rbdbgr-many-windows
        (rbdbgr-setup-windows-initially))

      (run-hooks 'rbdbgr-mode-hook))))


(defun rbdbgr-reset ()
  "Rbdbgr cleanup - remove debugger's internal buffers (frame, breakpoints, etc.)."
  (interactive)
  (rbdbgr-breakpoint-remove-all-icons)
  (dolist (buffer (buffer-list))
    (when (string-match "\\*rbdbgr-[a-z]+\\*" (buffer-name buffer))
      (let ((w (get-buffer-window buffer)))
        (when w
          (delete-window w)))
      (kill-buffer buffer))))

(defun rbdbgr-reset-keymaps()
  "This unbinds the special debugger keys of the source buffers."
  (interactive)
  (setcdr (assq 'rbdbgr-debugger-support-minor-mode minor-mode-map-alist)
	  rbdbgr-debugger-support-minor-mode-map-when-deactive))


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
