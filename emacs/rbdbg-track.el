;;; rbdbg-track.el --- Tracking the Ruby debugger from a shell

;;; Commentary:

;;  `rbdbg-track-mode' allows access to full debugger user interface
;;   for Ruby debugger sessions started in a standard shell window.
;;   `turn-on-rbdbg-track-mode' turns the mode on and
;;   `turn-off-rbdbg-track-mode' turns it off.
;;
;;; Customization:
;;  `rbdbg-track' sets whether file tracking is done by the shell prompt.
;;  `rbdbg-track-minor-mode-string' sets the mode indicator to show that
;;  tracking is in effect.
;;

;;; Code:

;; -------------------------------------------------------------------
;; Customizable variables.
;;

(defgroup rbdbg-track nil
  "Ruby debug and rbdbg file tracking by watching the shell prompt."
  :prefix "rbdbg-track"
  :group 'shell)

(defcustom rbdbg-track-minor-mode-string " rbdbg"
  "*String to use in the minor mode list when rbdbg-track is enabled."
  :type 'string
  :group 'rbdbg)


;; -------------------------------------------------------------------
;; Variables.
;;

(eval-when-compile
  (defconst rbdbg-track-char-range 10000
    "Max number of characters from end of buffer to search for stack entry."))


;; -------------------------------------------------------------------
;; Dependencies.
;;

(require 'comint)
(require 'custom)

(eval-when-compile
  (require 'cl)
  (setq load-path (cons nil load-path))
  (load "rbdbg-loc")
  (load "rbdbg-file")
  (load "rbdbgr-regexp")
  (setq load-path (cdr load-path)))

;; FIXME: add buffer local variables (in the process buffer) for:
;; rbdbg-last-output-start

; FIXME: Move this windowing routine into a file handling windowing.
(defun rbdbg-split-or-other-window()
  "Split the window if there is only one in the current
  frame. However if there is more than one window move to that"
  (interactive)
  ;; Anders code has more complicated logic for figuring out
  ;; which of serverl "other" windows is the one you want to switch
  ;; to.
  (if (one-window-p) (split-window) (other-window 1)))

(defun rbdbg-track-comint-output-filter-hook(text)
  "An output-filter hook custom for comint shells.  Find the file
indicated by the rbdbg location printed before a prompt.  The
parameter TEXT appears because it is part of the
comint-output-filter-functions API. Instead we use marks set in
buffer-local variables to extract text"

  ;; Instead of trying to piece things together from partial text
  ;; (which can be almost useless depending on Emacs version), we
  ;; monitor to the point where we have the next rbdbg prompt, and then
  ;; check all text from comint-last-input-end to process-mark.
  (let* ((curr-proc (get-buffer-process (current-buffer)))
	 (proc-mark (process-mark curr-proc))
	 (block-start (max comint-last-input-end 
			   (- proc-mark rbdbg-track-char-range))))
    (rbdbg-track-from-region block-start proc-mark)))

(defun rbdbg-track-from-region(from to)
  (interactive "r")
  (if (> from to)
      (let ((tem to)) (setq to from from tem)))
  (rbdbg-track-loc (buffer-substring from to)))

; FIXME: move somewhere else? Or maybe a top-level tracking UI will
; be created in another file
(defun rbdbg-track-loc(text)
  "Select position a buffer in the file indicated by scanning TEXT for a location.
We use `rbdbg-input-prompt-regexp' to find and parse the
location"
  ; FIXME rbdbgr-loc-regexp is for rbdbgr. Change to rbdbg-loc-regexp
  ; which will be generic and picked up from a buffer-local variable
  ; containing the "debugger" object.
  (if (string-match rbdbgr-loc-regexp text)
      (lexical-let* ((filename (match-string rbdbgr-loc-regexp-file-group text))
		     (lineno (string-to-number
			      (match-string rbdbgr-loc-regexp-line-group text)))
		     (loc (rbdbg-file-loc-from-line filename lineno)))
	(if (rbdbg-loc? loc)
	    (rbdbg-loc-goto loc 'rbdbg-split-or-other-window)
	  (message "%s" loc)))))

;; -------------------------------------------------------------------
;; The end.
;;

(provide 'rbdbg-track)

;;; Local variables:
;;; eval:(put 'rbdbg-debug-enter 'lisp-indent-hook 1)
;;; End:

;;; rbdbg-track.el ends here
