(eval-when-compile (require 'cl))

(defstruct rbdbg-loc filename line-number mark)

(defun rbdbg-loc-current()
  "Create a location object for the current buffer position."
  (make-rbdbg-loc :filename (buffer-file-name (current-buffer))
		  :line-number (line-number-at-pos) 
		  :mark (point-marker)))

; FIXME: could probably use a macro to define file-name, line-number
; and marker.

(defun rbdbg-loc-mark=(loc mark)
  "Return the filename stored in LOC or nil if LOC is not a rbdbg-loc."
  (setf (rbdbg-loc-mark loc) mark))

(defun rbdbg-loc-goto(loc &optional window-fn &rest args)
  "Goto the LOC which may involve switching buffers and moving
the point to the places indicated by LOC. In the process, the buffer
and marker inside loc may be updated. If WINDOW-FN and ARGS are given,
WINDOW-FN is called before switching bufffers"
  (if (rbdbg-loc-p loc) 
      (lexical-let* ((filename    (rbdbg-loc-filename loc))
		     (line-number (rbdbg-loc-line-number loc))
		     (marker      (rbdbg-loc-mark loc))
		     (buffer      (marker-buffer (or marker (make-marker)))))
	(if (not buffer)
	    (setq buffer (find-file-noselect filename)))
	(if buffer
	    (progn 
	      (if window-fn (apply window-fn args))
	      (switch-to-buffer buffer)
	      (if (not (and marker (marker-position marker)))
		  (progn 
		    (goto-line line-number)
		    (setq marker (rbdbg-loc-mark= loc (point-marker)))))
	      (if (and marker (marker-position marker)) 
		  (goto-char (marker-position marker)))))
	)))
