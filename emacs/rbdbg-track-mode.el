;;  `rbdbg-track-mode' tracks shell output 

(eval-when-compile
  (setq load-path (cons nil (cons ".." load-path)))
  (require 'cl)
  (load "rbdbg-track")
  (load "rbdbg-loc")
  (load "rbdbg-lochist")
  (load "rbdbg-file")
  (load "rbdbg-var")
  (load "rbdbg-window")
  (load "rbdbg-regexp")
  (setq load-path (cddr load-path)))
(require 'rbdbg-track)
(require 'rbdbg-regexp)

(defvar rbdbg-track-minor-mode nil
  "Non-nil if using rbdbg-track mode as a minor mode of some other mode.
Use the command `rbdbg-track-minor-mode' to toggle or set this variable.")

(defvar rbdbg-track-minor-mode-map
  (let ((map (make-sparse-keymap)))
    (define-key map [M-right]	'rbdbg-track-hist-newest)
    (define-key map [M-down]	'rbdbg-track-hist-newer)
    (define-key map [M-up]	'rbdbg-track-hist-older)
    (define-key map [M-S-down]	'rbdbg-track-hist-newest)
    (define-key map [M-S-up]	'rbdbg-track-hist-oldest)
    map)
  "Keymap used in `rbdbg-track-minor-mode'.")

(define-minor-mode rbdbg-track-mode
  "Minor mode for tracking ruby debugging inside a process shell."
  :init-value nil
  ;; :lighter " rbdbgr"   ;; indicator in the mode line.
  ;; The minor mode bindings.
  :global nil
  :group 'rbdbg
  :keymap rbdbg-track-minor-mode-map
  (if rbdbg-track-mode
      (progn
	(add-hook 'comint-output-filter-functions 
		  'rbdbg-track-comint-output-filter-hook)
	(add-hook 'eshell-output-filter-functions 
		  'rbdbg-track-eshell-output-filter-hook)
  
	;; FIXME: the following is customized for the debugger rbdbgr. 
	;; Other debuggers will be put in rbdbg-dbgr-pat-hash and the 
	;; below should be customizable for those debuggers by setting
	;; dbg-name accordingly. Put this in a subroutine.
	(rbdbg-track-set-debugger "rbdbgr")
	(run-mode-hooks 'rbdbg-track-mode-hook)
	)
    (progn
	(remove-hook 'comint-output-filter-functions 
		  'rbdbg-track-comint-output-filter-hook)
	(remove-hook 'eshell-output-filter-functions 
		    'rbdbg-track-eshell-output-filter-hook)
	)))

;; -------------------------------------------------------------------
;; The end.
;;

(provide 'rbdbg-track-mode)

;;; Local variables:
;;; eval:(put 'rbdbg-debug-enter 'lisp-indent-hook 1)
;;; End:

;;; rbdbgr-track.el ends here
