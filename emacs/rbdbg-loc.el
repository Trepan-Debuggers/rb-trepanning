(eval-when-compile (require 'cl))

(defun rbdbg-loc?(object)
  "Return t if OBJECT is a rbdbg position"
  (and (listp object) 
       (equal (length object) 4)
       (stringp (first object))
       (numberp (second object))
       (or (bufferp (third object))  (null (third object)))
       (or (markerp (fourth object)) (null (fourth object)))
       ))

(defun rbdbg-loc-new(filename line-number &optional buffer marker)
  "Create a location object for FILENAME and LINE-NUMBER. An optional 
MARKER can be used to record where this position is"
  (if (null marker) (setq marker (make-marker)))
  (let ((object (list filename line-number buffer marker)))
    (if (rbdbg-loc? object) object nil)))

(defun rbdbg-loc-current()
  "Create a location object for the current buffer position."
  (rbdbg-loc-new (buffer-file-name (current-buffer))
		 (line-number-at-pos) (current-buffer) (point-marker)))

; FIXME: could probably use a macro to define file-name, line-number
; and marker.

(defun rbdbg-loc-filename(loc)
  "Return the filename stored in LOC or nil if LOC is not a rbdbg-loc."
  (if (rbdbg-loc? loc) (first loc) nil))

(defun rbdbg-loc-line-number(loc)
  "Return the filename stored in LOC or nil if LOC is not a rbdbg-loc."
  (if (rbdbg-loc? loc) (second loc) nil))

(defun rbdbg-loc-buffer(loc)
  "Return the buffer stored in LOC or nil if LOC is not a rbdbg-loc."
  (if (rbdbg-loc? loc) (third loc) nil))

(defun rbdbg-loc-buffer-set(loc buffer)
  "Return the buffer stored in LOC or nil if LOC is not a rbdbg-loc."
  (if (rbdbg-loc? loc) (set (third loc) buffer)))

(defun rbdbg-loc-marker(loc)
  "Return the filename stored in LOC or nil if LOC is not a rbdbg-loc."
  (if (rbdbg-loc? loc) (fourth loc) nil))

(defun rbdbg-loc-marker-set(loc marker)
  "Return the filename stored in LOC or nil if LOC is not a rbdbg-loc."
  (if (rbdbg-loc? loc) (set (fourth loc) marker)))

(defun rbdbg-loc-goto(loc)
  "Goto the LOC which may involve switching buffers and moving
the point to the places indicated by LOC. In the process, the buffer
and marker inside loc may be updated"
  (if (rbdbg-loc? loc) 
      (lexical-let ((filename    (rbdbg-loc-filename loc))
		    (line-number (rbdbg-loc-line-number loc))
		    (buffer      (rbdbg-loc-buffer loc))
		    (marker      (rbdbg-loc-marker loc)))
	(if (not buffer)
	    (setq buffer (rbdbg-loc-buffer-set loc (find-file filename))))
	(if buffer
	    (progn 
	      (switch-to-buffer buffer)
	      (if (not marker)
		  (progn 
		    (goto-line line-number)
		    (rbdbg-loc-marker-set loc (point-marker)))
		(goto-char (marker-position marker)))
	      )))))
