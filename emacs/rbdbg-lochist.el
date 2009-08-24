;;; rbdbg-locring.el --- Ruby debugger location ring
;;; Commentary:

;; This file manages a ring of (recently stopped) positions to allow
;; the programmer to move back and forth between them.


;; FIXME? We don't have the buffer/file/marker positioning code motion
;; here yet. Should we? 

;;; Code:

(eval-when-compile (require 'cl))
(require 'rbdbg-loc)
(require 'ring)

(defcustom rbdbg-loc-hist-size 3  ; For testing. Should really be larger.
  "Size of rbdbg position history ring"
  :type 'integer
  :group 'rbdbg)

(defun rbdbg-loc-hist-new()
  (list (make-ring rbdbg-loc-hist-size) -1))

(defun rbdbg-loc-hist-ring(loc-hist)
  "The ring component of LOC-HIST"
  (first loc-hist))

(defun rbdbg-loc-hist-position(loc-hist)
  "The ring-position component of LOC-HIST"
  (second loc-hist))

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
      (message "dup item size is %s" (ring-length ring))
      (ring-insert-at-beginning ring item))))

(defun rbdbg-loc-hist-clear(loc-hist)
  "Clear out all source locations in LOC-HIST"
  (let ((ring (ring-ref (rbdbg-loc-hist-ring loc-hist)
			(rbdbg-loc-hist-position loc-hist))))
    (set (rbdbg-loc-hist-position loc-hist) -1)
    (while (not (ring-empty-p ring))
      (ring-remove ring))))


(defun rbdbg-loc-hist-set (loc-hist position)
  "Set LOC-HIST to POSITION in the stopping history"
  (set (rbdbg-loc-hist-position loc-hist) position))

(defun rbdbg-loc-hist-newest (loc-hist)
  "Set LOC-HIST position to the newest position."
  (set (rbdbg-loc-hist-position loc-hist) -1))
  
(defun rbdbg-locring-older (loc-hist)
  "Set LOC-HIST pisition to an older position."
    (if (equal (rbdbg-loc-hist-position loc-hist) 0)
	(message "At oldest - Will set to wrap to newest."))
    (set (rbdbg-loc-hist-position loc-hist) 
	 (ring-minus1 (rbdbg-loc-hist-position loc-hist)
		      (ring-length (rbdbg-loc-hist-ring loc-hist)))))

(defun rbdbg-locring-oldest (loc-hist)
  "Set LOC-HIST to the oldest stopping point."
  (set (rbdbg-loc-hist-position loc-hist) 0))

(provide 'rbdbg-locring)

;;; Local variables:
;;; eval:(put 'rbdbg-debug-enter 'lisp-indent-hook 1)
;;; End:

;;; rbdbg-lochist.el ends here
