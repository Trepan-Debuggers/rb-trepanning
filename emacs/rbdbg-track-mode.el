;;  `rbdbg-track-mode' tracks shell output 

(eval-when-compile
  (require 'cl)
  (setq load-path (cons nil (cons ".." load-path)))
  (load "rbdbg-track")
  (load "rbdbg-lochist")
  (load "rbdbg-var")
  (setq load-path (cddr load-path)))
(require 'rbdbg-track)

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
  

  ;; FIXME: the following is customized for the debugger rbdbgr. 
  ;; Other debuggers will be put in rbdbg-dbgr-pat-hash and the 
  ;; below should be customizable for those debuggers by setting
  ;; dbg-name accordingly. Put this in a subroutine.
  (lexical-let* ((dbg-name "rbdbgr")
		 (loc-pat (gethash dbg-name rbdbg-dbgr-pat-hash)))
    (setq rbdbg-dbgr (make-rbdbg-dbgr
		      :name dbg-name
		      :loc-regexp (rbdbg-dbgr-loc-pat-regexp     loc-pat)
		      :file-group (rbdbg-dbgr-loc-pat-file-group loc-pat)
		      :line-group (rbdbg-dbgr-loc-pat-line-group loc-pat)
		      :loc-hist   (make-rbdbg-loc-hist))))
    
    (run-mode-hooks 'rbdbg-track-mode-hook)
;; FIXME: add buffer local variables (in the process buffer) for:
;; rbdbg-last-output-start
)

