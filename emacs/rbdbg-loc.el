(defun rbdbg-loc?(object)
  "Return t if OBJECT is a rbdbg position"
  (and (listp object) 
       (equal (length object) 3)
       (stringp (first object))
       (numberp (second object))
       (markerp (third object))))

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

(defun rbdbg-loc-filename(object)
  "Return the filename stored in OBJECT or nil if object is not a rbdbg-loc."
  (if (rbdbg-loc? object) (first object) nil))

(defun rbdbg-loc-line-number(object)
  "Return the filename stored in OBJECT or nil if object is not a rbdbg-loc."
  (if (rbdbg-loc? object) (second object) nil))

(defun rbdbg-loc-marker(object)
  "Return the filename stored in OBJECT or nil if object is not a rbdbg-loc."
  (if (rbdbg-loc? object) (third object) nil))

(provide 'rbdbg-loc)
