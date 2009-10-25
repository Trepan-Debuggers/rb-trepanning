;;; rbdbgr-track-mode.el --- Ruby "rbdbgr" Debugger tracking a comint
;;; or eshell buffer.

(eval-when-compile
  (require 'cl))

(setq load-path (cons nil (cons "." load-path)))
(require 'rbdbg-track-mode)
(require 'rbdbgr-core)
(setq load-path (cddr load-path))

  
(defvar rbdbgr-track-mode nil
  "Non-nil if using rbdbgr-track mode as a minor mode of some other mode.
Use the command `rbdbgr-track-mode' to toggle or set this variable.")

(defvar rbdbgr-track-mode-map
  (let ((map (make-sparse-keymap)))
    (define-key map [C-c !]	'rbdbgr-goto-dollarbang-traceback-line)
    (define-key map [C-c e]	'rbdbgr-goto-traceback-line)
    map)
  "Keymap used in `rbdbgr-track-mode'.")

(define-minor-mode rbdbgr-track-mode
  "Minor mode for tracking ruby debugging inside a process shell."
  :init-value nil
  :lighter " rbdbgr"   ;; indicator in the mode line.
  ;; The minor mode bindings.
  :global nil
  :group 'rbdbgr
  :keymap rbdbgr-track-mode-map
  
  (rbdbg-track-set-debugger "rbdbgr")
  (if rbdbgr-track-mode
      (progn (rbdbg-track-mode 't)
	     (run-mode-hooks 'rbdbgr-track-mode-hook))
    (rbdbg-track-mode nil)))
  

;; -------------------------------------------------------------------
;; The end.
;;

(provide 'rbdbgr-track-mode)

;;; Local variables:
;;; eval:(put 'rbdbg-debug-enter 'lisp-indent-hook 1)
;;; End:

;;; rbdbgr-track.el ends here
