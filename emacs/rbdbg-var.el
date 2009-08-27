;;; rbdbgr-var.el --- debugger variables (other than regexps)

; The debugger "object"/structure specific to a process buffer.
(defstruct rbdbg-dbgr 
  loc-hist    ; ring of locations seen in the course of execution
              ; see rbdbg-lochist
  loc-regexp  ; regular expression matching a location 
  loc-regexp-file-group ; position of file in loc-regexp
  loc-regexp-line-group ; position of line number in loc-regexp
)

(defvar rbdbg-dbgr (make-rbdbg-dbgr)
  "Debugger object for a process buffer.")
(make-variable-buffer-local 'rbdbg-dbgr)

(provide 'rbdbgr-var)

;;; Local variables:
;;; eval:(put 'rbdbgr-debug-enter 'lisp-indent-hook 1)
;;; End:

;;; rbdbgr-vars.el ends here
