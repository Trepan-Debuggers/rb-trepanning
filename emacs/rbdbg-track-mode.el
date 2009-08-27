;;  `rbdbg-track-mode' tracks shell output 

(eval-when-compile
  (require 'cl)
  (setq load-path (cons nil (cons ".." load-path)))
  (load "rbdbg-track")
  (load "rbdbg-var")
  (setq load-path (cddr load-path)))

(defvar rbdbg-track-minor-mode nil
  "Non-nil if using rbdbgr-track mode as a minor mode of some other mode.
Use the command `rbdbg-minor-mode' to toggle or set this variable.")

(defvar rbdbg-track-minor-mode-map
  (let ((map (make-sparse-keymap)))
    (define-key map [M-right]	'rbdbg-track-hist-newest)
    (define-key map [M-down]	'rbdbg-track-hist-newer)
    (define-key map [M-up]	'rbdbg-track-hist-older)
    (define-key map [M-S-down]	'rbdbg-track-hist-newest)
    (define-key map [M-S-up]	'rbdbg-track-hist-oldest)
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
  (setq rbdbg-dbgr (make-rbdbg-dbgr))
  (setf (rbdbg-dbgr-loc-hist rbdbg-dbgr) (make-rbdbg-loc-hist))
  (run-mode-hooks 'rbdbg-track-mode-hook)
;; FIXME: add buffer local variables (in the process buffer) for:
;; rbdbg-last-output-start
)

