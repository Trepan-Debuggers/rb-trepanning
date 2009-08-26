(eval-when-compile (require 'cl))

(defun rbdbg-loc?(object)
  "Return t if OBJECT is a rbdbg position"
  (and (listp object) 
       (equal (length object) 3)
       (stringp (first object))
       (numberp (second object))
       (or (markerp (third object)) (null (third object)))
       ))

(defun rbdbg-loc-new(filename line-number &optional marker)
  "Create a location object for FILENAME and LINE-NUMBER. An optional 
MARKER can be used to record where this position is"
  (if (null marker) (setq marker (make-marker)))
  (let ((object (list filename line-number marker)))
    (if (rbdbg-loc? object) object nil)))

(defun rbdbg-loc-current()
  "Create a location object for the current buffer position."
  (rbdbg-loc-new (buffer-file-name (current-buffer))
		 (line-number-at-pos) (point-marker)))

; FIXME: could probably use a macro to define file-name, line-number
; and marker.

(defun rbdbg-loc-filename(loc)
  "Return the filename stored in LOC or nil if LOC is not a rbdbg-loc."
  (if (rbdbg-loc? loc) (first loc) nil))

(defun rbdbg-loc-line-number(loc)
  "Return the filename stored in LOC or nil if LOC is not a rbdbg-loc."
  (if (rbdbg-loc? loc) (second loc) nil))

(defun rbdbg-loc-marker(loc)
  "Return the filename stored in LOC or nil if LOC is not a rbdbg-loc."
  (if (rbdbg-loc? loc) (third loc) nil))

(defun rbdbg-loc-marker=(loc marker)
  "Return the filename stored in LOC or nil if LOC is not a rbdbg-loc."
  (if (rbdbg-loc? loc) (setf (third loc) marker)))

(defun rbdbg-loc-goto(loc &optional window-fn args)
  "Goto the LOC which may involve switching buffers and moving
the point to the places indicated by LOC. In the process, the buffer
and marker inside loc may be updated. If WINDOW-FN and ARGS are given,
WINDOW-FN is called before switching bufffers"
  (if (rbdbg-loc? loc) 
      (lexical-let* ((filename    (rbdbg-loc-filename loc))
		     (line-number (rbdbg-loc-line-number loc))
		     (marker      (rbdbg-loc-marker loc))
		     (buffer      (marker-buffer marker)))
	(if (not buffer)
	    (setq buffer (find-file-noselect filename)))
	(if buffer
	    (progn 
	      (if window-fn (funcall window-fn args))
	      (switch-to-buffer buffer)
	      (if (not (and marker (marker-position marker)))
		  (progn 
		    (goto-line line-number)
		    (setq marker (rbdbg-loc-marker= loc (point-marker)))))
	      (if (and marker (marker-position marker)) 
		  (goto-char (marker-position marker)))))
	)))
