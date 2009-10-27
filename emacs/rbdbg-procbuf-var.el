;;; rbdbg-procbuf-var.el --- debugger variables (other than regexps) for
;;; a process buffer
(eval-when-compile (require 'cl))

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

(provide 'rbdbg-procbuf-var)

;;; Local variables:
;;; eval:(put 'rbdbgr-debug-enter 'lisp-indent-hook 1)
;;; End:

;;; rbdbg-vars.el ends here
