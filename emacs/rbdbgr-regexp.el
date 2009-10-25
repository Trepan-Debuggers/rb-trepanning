;;; rdebug-regexp.el --- Ruby debugger regular expressions

;; -------------------------------------------------------------------
;; Variables defining regular expressions (regexp:s).
;;
(eval-when-compile
  (setq load-path (cons nil (cons ".." load-path)))
  (require 'cl)
  (load "rbdbg-regexp") ; for make-rbdbg-dbg-loc-pat
  (setq load-path (cddr load-path)))


(defvar rbdbgr-dbgr-pat-hash (make-hash-table :test 'equal)
  "Hash key is the what kind of pattern we want to match: traceback, prompt, etc. 
The values of a hash entry is a rbdbg-dbgr-loc-pat struct")

;;  Regular expression that describes a rbdbgr command prompt
(setf (gethash "prompt" rbdbgr-dbgr-pat-hash)
      (make-rbdbg-dbgr-loc-pat
       :regexp ".. (.*?\\(?:via \\)?\\([-a-zA-Z0-9_/.]+\\):\\([0-9]+\\))"
       :file-group 1
       :line-group 2))

;;  Regular expression that describes a Ruby traceback line.
(setf (gethash "traceback" rbdbgr-dbgr-pat-hash)
      (make-rbdbg-dbgr-loc-pat
       :regexp "^[ \t]+from \\([^:]+\\):\\([0-9]+\\)\\(?: in `.*'\\)?"
       :file-group 1
       :line-group 2))

;;  Regular expression that describes a Ruby $! string
(setf (gethash "dollar-bang" rbdbgr-dbgr-pat-hash)
      (make-rbdbg-dbgr-loc-pat
       :regexp "^[ \t]*[[]?\\(.+\\):\\([0-9]+\\):in `.*'"
       :file-group 1
       :line-group 2))


(provide 'rbdbgr-regexp)

;;; Local variables:
;;; eval:(put 'rbdbgr-debug-enter 'lisp-indent-hook 1)
;;; End:

;;; rbdbgr-regexp.el ends here
