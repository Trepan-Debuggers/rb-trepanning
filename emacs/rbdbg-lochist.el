;;; rbdbg-locring.el --- Ruby debugger location ring
;;; Commentary:

;; This file manages a ring of (recently stopped) positions to allow
;; the programmer to move between them.


;;; Code:

(eval-when-compile 
  (require 'cl)
  (require 'ring)
  (setq load-path (cons nil (cons ".." load-path)))
  (load "rbdbg-loc")
  (setq load-path (cddr load-path)))


(defcustom rbdbg-loc-hist-size 10  ; For testing. Should really be larger.
  "Size of rbdbg position history ring"
  :type 'integer
  :group 'rbdbg)

(defstruct rbdbg-loc-hist
  "A list of source-code positions recently encountered"
  (position -1 :type integer) 
  (ring (make-ring rbdbg-loc-hist-size) :type (type-of make-ring 0)))
  
(defun rbdbg-loc-hist-item-at(loc-hist position)
  "Get the current item stored at POSITION of the ring
component in LOC-HIST"
  (lexical-let ((ring (rbdbg-loc-hist-ring loc-hist)))
    (if (ring-empty-p ring)
	nil
      (ring-ref ring position))))

(defun rbdbg-loc-hist-item(loc-hist)
  "Get the current item of LOC-HIST at the position previously set"
  (rbdbg-loc-hist-item-at 
   loc-hist
   (rbdbg-loc-hist-position loc-hist)))

(defun rbdbg-loc-hist-add(loc-hist item)
  "Add FRAME to LOC-HIST"
  ;; Switching frames shouldn't save a new ring
  ;; position. Also make sure no position is different.
  ;; Perhaps duplicates should be controlled by an option.
  (let ((ring (rbdbg-loc-hist-ring loc-hist)))
    (unless (equal (rbdbg-loc-hist-item loc-hist) item)
      (ring-insert-at-beginning ring item))))

(defun rbdbg-loc-hist-clear(loc-hist)
  "Clear out all source locations in LOC-HIST"
  (let ((ring (ring-ref (rbdbg-loc-hist-ring loc-hist)
			(rbdbg-loc-hist-position loc-hist))))
    (setf (rbdbg-loc-hist-position loc-hist) -1)
    (while (not (ring-empty-p ring))
      (ring-remove ring))))


(defun rbdbg-loc-hist-set (loc-hist position)
  "Set LOC-HIST to POSITION in the stopping history"
  (setf (rbdbg-loc-hist-position loc-hist) position))

; FIXME: add numeric arg? 
(defun rbdbg-loc-hist-newer (loc-hist)
  "Set LOC-HIST position to an newer position."
    (if (equal (rbdbg-loc-hist-position loc-hist) -1)
	(message "At newest - Will set to wrap to oldest."))
    (setf (rbdbg-loc-hist-position loc-hist) 
	 (ring-plus1 (rbdbg-loc-hist-position loc-hist)
		      (ring-length (rbdbg-loc-hist-ring loc-hist)))))

(defun rbdbg-loc-hist-newest (loc-hist)
  "Set LOC-HIST position to the newest position."
  (setf (rbdbg-loc-hist-position loc-hist) -1))
  
; FIXME: add numeric arg? 
(defun rbdbg-loc-hist-older (loc-hist)
  "Set LOC-HIST position to an older position."
    (if (equal (rbdbg-loc-hist-position loc-hist) 0)
	(message "At oldest - Will set to wrap to newest."))
    (setf (rbdbg-loc-hist-position loc-hist) 
	 (ring-minus1 (rbdbg-loc-hist-position loc-hist)
		      (ring-length (rbdbg-loc-hist-ring loc-hist)))))

(defun rbdbg-loc-hist-oldest (loc-hist)
  "Set LOC-HIST to the oldest stopping point."
  (setf (rbdbg-loc-hist-position loc-hist) 0))

(provide 'rbdbg-loc-hist)

;;; Local variables:
;;; eval:(put 'rbdbg-debug-enter 'lisp-indent-hook 1)
;;; End:

;;; rbdbg-lochist.el ends here
