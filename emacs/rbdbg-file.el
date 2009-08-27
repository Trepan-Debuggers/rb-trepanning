; Should rbdbg-file-loc-from-line be here or elsewhere?
(eval-when-compile
  (require 'cl)
  (setq load-path (cons nil (cons ".." load-path)))
  (load "rbdbg-loc")
  (setq load-path (cddr load-path)))


(defun rbdbg-file-line-count(filename)
  "Return the number of lines in file FILENAME, or nil FILENAME can't be
found"
  (lexical-let ((saved-buffer (current-buffer))
		(result nil))
    (if (file-exists-p filename)
	(progn (find-file filename)
	       (setq result (line-number-at-pos (point-max)))))
    (switch-to-buffer saved-buffer)
    result))

(defun rbdbg-file-loc-from-line(filename line-number)
  "Return a rbdbg-loc for FILENAME and LINE-NUMBER

If we're unable find the source code we return a string describing the
problem as best as we can determine."

  (if (file-exists-p filename)
      (if (integerp line-number)
	  (if (> line-number 0)
	      (lexical-let ((line-count))
		(if (setq line-count (rbdbg-file-line-count filename))
		    (if (> line-count line-number)
			; And you thought we'd never get around to
			; doing something other than validation? 
			(make-rbdbg-loc :filename    filename 
					:line-number line-number
					:marker      (make-marker))
		      (format "File %s has only %d lines. (Line %d requested.)"
			      filename line-count line-number))
		  (format "Problem getting line count for file `%s'" filename)))
	    (format "line number %s should be greater than 0" line-number))
	(format "%s is not an integer" line-number))
    (format "File named `%s' not found" filename)))

(provide 'rbdbg-file)

