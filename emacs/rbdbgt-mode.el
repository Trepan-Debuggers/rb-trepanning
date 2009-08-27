;;  `rbdbg-track-mode' tracks shell output 

(eval-when-compile
  (require 'cl)
  (setq load-path (cons nil load-path))
  (load "rbdbg-track")
  (setq load-path (cdr load-path)))

(defvar rbdbg-track-minor-mode nil
  "Non-nil if using rbdbgr-track mode as a minor mode of some other mode.
Use the command `rbdbg-minor-mode' to toggle or set this variable.")

(defvar rbdbg-track-minor-mode-map
  (let ((map (make-sparse-keymap)))
    (define-key map "\C-cg"	      'rbdbg-goto-loc)
    map)
  "Keymap for rbdbgr-track minor mode.")

(define-minor-mode rbdbgr-track-mode
  "Minor mode for tracking ruby debugging inside a process shell."
  :init-value nil
  :lighter " rbdbgr"   ;; indicator in the mode line.
  ;; The minor mode bindings.
  :global nil
  :group 'rbdbg
  :keymap rbdbg-track-minor-mode-map
  (add-hook 'comint-output-filter-functions 
	    'rbdbg-track-comint-output-filter-hook)
  (add-hook 'eshell-output-filter-functions 
	    'rbdbg-track-eshell-output-filter-hook)
  ;; (run-mode-hooks 'rdebug-track-mode-hook)
;; FIXME: add buffer local variables (in the process buffer) for:
;; rbdbg-last-output-start
)

