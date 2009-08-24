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

(defcustom rbdbg-track-tracking? t
  "*Controls whether the rbdbg-track feature is enabled or not.
When non-nil, rbdbg-track is enabled in all comint-based buffers,
e.g. shell buffers and the *Ruby* buffer.  When using rbdbg to debug a
Ruby program, rbdbg-track notices the rbdbg prompt and displays the
source file and line that the program is stopped at, much the same way
as gud-mode does for debugging C programs with gdb."
  :type 'boolean
  :group 'rbdbg)
(make-variable-buffer-local 'rbdbg-track-tracking?)

(defcustom rbdbg-track-minor-mode-string " rbdbg"
  "*String to use in the minor mode list when rbdbg-track is enabled."
  :type 'string
  :group 'rbdbg)


;; -------------------------------------------------------------------
;; Variables.
;;

(defconst rbdbg-track-track-range 10000
  "Max number of characters from end of buffer to search for stack entry.")


;; -------------------------------------------------------------------
;; Dependencies.
;;

(require 'comint)
(require 'custom)
(require 'cl)

(load-file "./rbdbg-loc.el")
(load-file "./rbdbgr-regexp.el")


;; -------------------------------------------------------------------
;; rbdbg track -- support for attaching the `rbdbg' ruby debugger to
;; a process running in a shell buffer.
;;

(defun rbdbg-track-comint-hook(text)
  "Find the file indicated by the rbdbg location printed before a prompt.
Activity is disabled if the buffer-local variable
`rbdbg-track-tracking?' is nil. The parameter TEXT appears because 
it is part of the comint-output-filter-functions API. Instead we use marks
set in buffer-local variables to extract text"  

  ;; Instead of trying to piece things together from partial text
  ;; (which can be almost useless depending on Emacs version), we
  ;; monitor to the point where we have the next rbdbg prompt, and then
  ;; check all text from comint-last-input-end to process-mark.
  (let ((curr-proc (get-buffer-process (current-buffer))))
    (if (and curr-proc rbdbg-track-tracking?)
	(let* ((proc-mark (process-mark curr-proc))
               (block-start (max comint-last-input-end
                                 (- proc-mark rbdbg-track-track-range)))
               (block-str (buffer-substring block-start proc-mark)))
	  (rbdbg-track-find-loc block-str)))))

(defun rbdbg-track-find-loc(text)
  "Show the file indicated by the rbdbg stack entry line, in a separate window.
Activity is disabled if the buffer-local variable
`rbdbg-track-tracking?' is nil.

We depend on the rbdbg input prompt matching `rbdbg-input-prompt-regexp'
at the beginning of the line."
  ; FIXME rbdbgr-position-regexp is for rbdbgr. rbdbg-position-regexp
  ; will be generic and picked up in a buffer-local variable.
  (if (string-match rbdbgr-position-regexp text)
      (lexical-let* ((filename (match-string rbdbgr-marker-regexp-file-group text))
		     (lineno (string-to-number
			      (match-string rbdbgr-marker-regexp-line-group text)))
		     (loc (rbdbg-track-find-loc-from-match filename lineno)))
	(if (rbdbg-loc? loc)
	  (progn (message "do stuff"))
	  (progn (message "%s" loc))))))

(defun rbdbg-track-find-loc-from-match(filename line-number)
  "Return a rbdbg-loc for FILENAME nd LINE-NUMBER

We look first to visit the file indicated in the trace.

Failing that, we look for the most recently visited ruby-mode buffer
with the same name or having having the named function.

If we're unable find the source code we return a string describing the
problem as best as we can determine."

  (if (file-exists-p filename)
      (rbdbg-loc-new filename line-number)
    (format "Not found: %s" filename)))



;; -------------------------------------------------------------------
;; The end.
;;

(provide 'rbdbg-track)

;;; Local variables:
;;; eval:(put 'rbdbg-debug-enter 'lisp-indent-hook 1)
;;; End:

;;; rbdbg-track.el ends here
