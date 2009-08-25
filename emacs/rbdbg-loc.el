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

(defun rbdbg-loc-filename(object)
  "Return the filename stored in OBJECT or nil if object is not a rbdbg-loc."
  (if (rbdbg-loc? object) (first object) nil))

(defun rbdbg-loc-line-number(object)
  "Return the filename stored in OBJECT or nil if object is not a rbdbg-loc."
  (if (rbdbg-loc? object) (second object) nil))

(defun rbdbg-loc-buffer(object)
  "Return the buffer stored in OBJECT or nil if object is not a rbdbg-loc."
  (if (rbdbg-loc? object) (third object) nil))

(defun rbdbg-loc-marker(object)
  "Return the filename stored in OBJECT or nil if object is not a rbdbg-loc."
  (if (rbdbg-loc? object) (fourth object) nil))

(provide 'rbdbg-loc)
