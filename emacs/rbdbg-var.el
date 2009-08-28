;;; rbdbgr-var.el --- debugger variables (other than regexps)
(require 'cl)

(defstruct rbdbg-dbgr
  "The debugger object/structure specific to a process buffer."
  (name       :type string) ; Name of debugger
  (loc-regexp :type string) ; Location regular expression string
  ; FIXME: use include?
  (file-group :type integer)
  (line-group :type integer)
  (loc-hist)    ; ring of locations seen in the course of execution
              ; see rbdbg-lochist
)

(defvar rbdbg-dbgr (make-rbdbg-dbgr
		    :name "unknown-debugger-name"
		    :loc-regexp nil
		    :file-group -1
		    :line-group -1
		    :loc-hist   nil)
  "Debugger object for a process buffer.")
(make-variable-buffer-local 'rbdbg-dbgr)

(provide 'rbdbgr-var)

;;; Local variables:
;;; eval:(put 'rbdbgr-debug-enter 'lisp-indent-hook 1)
;;; End:

;;; rbdbgr-vars.el ends here
