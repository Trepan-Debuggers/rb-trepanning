;;; rbdbgr-regexp.el --- Ruby debugger regular expressions

;; Here we have regular expressions and names for matched patterns
;; of those regular expressions.

;;; Code:

;; -------------------------------------------------------------------
;; Variables defining regular expressions (regexp:s).
;;

(defconst rbdbgr-position-regexp
  ".. (\\([-a-zA-Z0-9_/.]*\\):\\([0-9]+\\))"
  "Regular expression for a rbdbgr position.")

(defconst rbdbgr-marker-regexp-file-group 1
  "Group position in `rbdbgr-position-regexp' that matches the file name.")

(defconst rbdbgr-marker-regexp-line-group 2
  "Group position in `rbdbgr-position-regexp' that matches the line number.")

(provide 'rbdbgr-regexp)

;;; Local variables:
;;; eval:(put 'rbdbgr-debug-enter 'lisp-indent-hook 1)
;;; End:

;;; rbdbgr-regexp.el ends here
