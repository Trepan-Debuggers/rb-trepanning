;;; rbdbg-locring.el --- Ruby debugger location ring
;;; Commentary:

;; This file manages a ring of (recently stopped) positions to allow
;; the programmer to move back and forth between them.

;;; Code:

(require 'rbdbg-loc)

(defcustom rbdbg-location-ring-size 3
  "Size of rbdbg position history ring"
  :type 'integer
  :group 'rbdbg)

(defun rbdbg-location-history-new()
  (list (make-ring rbdbg-location-ring-size) -1))

(defun rbdbg-location-history-ring(location-history)
  "The ring component of LOCATION-HISTORY"
  (first location-history))

(defun rbdbg-location-history-position(location-history)
  "The ring-position component of LOCATION-HISTORY"
  (second location-history))

(defun rbdbg-location-history-item-at(location-history position)
  "The current frame stored at POSITION of the ring
component in LOCATION-HISTORY"
  (lexical-let ((ring (rbdbg-location-history-ring location-history)))
    (if (ring-empty-p ring)
	nil
      (ring-ref ring position))))

(defun rbdbg-location-history-item(location-history)
  "The current frame of LOCATION-HISTORY"
  (rbdbg-location-history-item-at 
   location-history
   (rbdbg-location-history-position location-history)))

(defun rbdbg-location-history-add (location-history item)
  "Add FRAME to LOCATION-HISTORY"
  ;; Switching frames shouldn't save a new ring
  ;; position. Also make sure no position is different.
  ;; Perhaps duplicates should be controlled by an option.
  (let ((ring (rbdbg-location-history-ring (location-history))))
    (unless (equal (rbdbg-location-history-item location-history) item)
      (ring-insert-at-beginning ring item))))

(defun rbdbg-location-history-clear (location-history)
  "Clear out all source locations in LOCATION-HISTORY"
  (interactive)
  (let ((ring (ring-ref (rbdbg-location-history-ring location-history)
			(rdbgr-location-history-position location-history))))
    (set (location-ring-position location-history) -1)N
    (while (not (ring-empty-p ring))
      (ring-remove ring))))

(defun rbdbg-locring-goto (ring-position)
  "Go the source position RING-POSITION in the stopping history"
  (interactive "NSource location ring position (0 is oldest): ")
  (with-current-buffer gud-comint-buffer
    (setq rbdbg-location-ring-index ring-position)
    (let* ((item (ring-ref rbdbg-location-ring ring-position))
	   (file (car item))
	   (line (cdr item)))
      (when file
	(rbdbg-display-line file line)
	(message (format "%d %s:%d" rbdbg-location-ring-index
			 file line))))))
    
(defun rbdbg-locring-newer ()
  "Cycle through source location stopping history to get the next newer (more recently visited) location."
  (interactive)
  (with-current-buffer gud-comint-buffer
    (if (equal (+ 1 rbdbg-location-ring-index)
	       (ring-length rbdbg-location-ring))
	(progn
	  (message "At newest - Will set to wrap to oldest.")
	  (setq rbdbg-location-ring-index -1))
      ;; else
      (rbdbg-locring-goto
       (if (> rbdbg-location-ring-index
	      (ring-length rbdbg-location-ring))
	   0
	 ;; else
	 (ring-plus1 rbdbg-location-ring-index
		     (ring-length rbdbg-location-ring)))))))

(defun rbdbg-locring-newest ()
  "Go to the source location of the first stopping point."
  (interactive)
  (rbdbg-locring-goto (- (ring-length rbdbg-location-ring) 1)))
  
(defun rbdbg-locring-older ()
  "Cycle through source location stopping history to get the next older (least recently visited) location."
  (interactive)
  (with-current-buffer gud-comint-buffer
    (if (equal rbdbg-location-ring-index 0)
	(progn
	  (message "At oldest - Will set to wrap to newest.")
	  (setq rbdbg-location-ring-index
		(+ 1 (ring-length rbdbg-location-ring))))
      ;; else
      (rbdbg-locring-goto
       (if (or (not rbdbg-location-ring-index)
	       (< rbdbg-location-ring-index 0))
	   0
	 ;; else
	 (ring-minus1 rbdbg-location-ring-index
		      (ring-length rbdbg-location-ring)))))))

(defun rbdbg-locring-oldest ()
  "Go to the oldest source position location."
  (interactive)
  (ring-ref rbdbg-location-ring 0))

(provide 'rbdbg-locring)

;;; Local variables:
;;; eval:(put 'rbdbg-debug-enter 'lisp-indent-hook 1)
;;; End:

;;; rbdbg-locring.el ends here
