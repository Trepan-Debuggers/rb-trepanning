;;; rbdbgr-regexp.el --- Debugger regular expressions

;; Here we have regular expressions and names for matched patterns
;; of those regular expressions.

;;; Code:

;; -------------------------------------------------------------------
;; Variables defining regular expressions (regexp:s).
;;

(defstruct rbdbg-dbgr-loc-pat
  "Information to match and extract a file, line number location from
a string output by a debugger"
  (regexp     :type string)
  (file-group :type integer)
  (line-group :type integer))

(defvar rbdbg-dbgr-pat-hash (make-hash-table :test 'equal)
  "Hash indexed by debugger name of rbdbg-dbgr-pat")

(setf (gethash "rbdbgr" rbdbg-dbgr-pat-hash)
      (make-rbdbg-dbgr-loc-pat
       :regexp ".. (\\([-a-zA-Z0-9_/.]*\\):\\([0-9]+\\))"
       :file-group 1
       :line-group 2))

(provide 'rbdbgr-regexp)

;;; Local variables:
;;; eval:(put 'rbdbgr-debug-enter 'lisp-indent-hook 1)
;;; End:

;;; rbdbgr-regexp.el ends here
