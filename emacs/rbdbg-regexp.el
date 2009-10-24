;;; rbdbg-regexp.el --- Debugger regular expressions for many kinds of
;;  debuggers

;; Here we have regular expressions and names for matched patterns
;; of those regular expressions.

;;; Code:

;; -------------------------------------------------------------------
;; Variables defining regular expressions (regexp:s).
;;

(defstruct rbdbg-dbgr-loc-pat
  "Information to match and extract a file and line number location from
a string output by a debugger inside a process shell"
  (regexp     :type string)
  (file-group :type integer)
  (line-group :type integer))

(defvar rbdbg-dbgr-pat-hash (make-hash-table :test 'equal)
  "Hash key is the debugger name, a string. The values of a hash entry
  is a rbdbg-dbgr-loc-pat struct")

;; FIXME? If this file gets too long and cumbersome, split each debugger
;; section to its own file.

; Create one for the Ruby 1.9 debugger "rbdbgr". 
(setf (gethash "rbdbgr" rbdbg-dbgr-pat-hash)
      (make-rbdbg-dbgr-loc-pat
       :regexp ".. (\\(?:.+ \\(?:via\\|remapped\\) \\)?\\(.+\\):\\([0-9]+\\))"
       :file-group 1
       :line-group 2))

; Create one for ruby-debug
(setf (gethash "rdebug" rbdbg-dbgr-pat-hash)
      (make-rbdbg-dbgr-loc-pat
       :regexp "\\(?:source \\)?\\(\\(?:[a-zA-Z]:\\)?[^:\n]*\\):\\([0-9]*\\).*\n"
       :file-group 2
       :line-group 3))

; Now one for the Python debugger "pydbgr". 
(setf (gethash "pydbgr" rbdbg-dbgr-pat-hash)
      (make-rbdbg-dbgr-loc-pat
       :regexp "^(\\(\\(?:[a-zA-Z]:\\)?[-a-zA-Z0-9_/.\\\\ ]+\\):\\([0-9]+\\))"
       :file-group 1
       :line-group 2))

; And the older Python debugger "pydb". 
(setf (gethash "pydb" rbdbg-dbgr-pat-hash)
      (make-rbdbg-dbgr-loc-pat
       :regexp "^(\\(\\(?:[a-zA-Z]:\\)?[-a-zA-Z0-9_/.\\\\ ]+\\):\\([0-9]+\\))"
       :file-group 1
       :line-group 2))

; Create one for the Korn Shell debugger "kshdb". 
(setf (gethash "kshdb" rbdbg-dbgr-pat-hash)
      (make-rbdbg-dbgr-loc-pat
       :regexp "\\(^\\|\n\\)(\\([^:]+\\):\\([0-9]*\\))"
       :file-group 2
       :line-group 3))

; And Z Shell debugger "zshdb". 
(setf (gethash "zshdb" rbdbg-dbgr-pat-hash)
      (make-rbdbg-dbgr-loc-pat
       :regexp "\\(^\\|\n\\)(\\([^:]+\\):\\([0-9]*\\))"
       :file-group 2
       :line-group 3))

; One for the Bash debugger "bashdb". 
(setf (gethash "bashdb" rbdbg-dbgr-pat-hash)
      (make-rbdbg-dbgr-loc-pat
       :regexp "\\(^\\|\n\\)(\\([^:]+\\):\\([0-9]*\\))"
       :file-group 2
       :line-group 3))

; And finally for GNU Make + debugger "remake". 
(setf (gethash "remake" rbdbg-dbgr-pat-hash)
      (make-rbdbg-dbgr-loc-pat
       :regexp "\\(?:^\\|\n\\)(\\(\\(?:[a-zA-Z]:\\)?[-a-zA-Z0-9_/.\\\\ ]+\\):\\([0-9]+\\))"
       :file-group 1
       :line-group 2))

(provide 'rbdbgr-regexp)

;;; Local variables:
;;; eval:(put 'rbdbgr-debug-enter 'lisp-indent-hook 1)
;;; End:

;;; rbdbgr-regexp.el ends here
