;;
;; Convert Oracle raw values into UUID and back
;;
;; UUID: 550e8400-e29b-41d4-a716-446655440000
;; RAW:      550E8400E29B41D4A716446655440000
;;
;; (load "~/Documents/lisp-scripts/raw-uuid.el")

(defun fa-oracle-uuid-convert ()
  "Convert raw/uuid value"
  (interactive)
  (fa--uuid-convert-region-impl (fa--find-uuid-start) (fa--find-uuid-end)))

(defun fa-oracle-uuid-convert-region ()
  "Convert value from RAW to UUID and vice versa"
  (interactive)
  (fa--uuid-convert-region-impl (region-beginning) (region-end)))

(defun fa--uuid-convert-region-impl (beginning end)
  "Convert value from RAW to UUID and vice versa"
  (let* ((str (buffer-substring-no-properties beginning end))
	 (str-len (length str))
	 (converted-str str))
    (cond ((equal str-len 36) ; UUID
	   (setq converted-str (fa--uuid-to-raw str)))
	  ((equal str-len 32) ; RAW
	   (setq converted-str (fa--raw-to-uuid str)))
	  (t
	   (error "Not a valid RAW or plain UUID")))
    (delete-region beginning end)
    (insert converted-str)))

(defun fa--uuid-to-raw (uuid)
  "Convert UUID into RAW"
  (upcase (string-replace "-" "" uuid)))

(defun fa--raw-to-uuid (raw-uuid)
  "Convert RAW into a UUID"
  (let* ((uuid-first  (substring raw-uuid 0 8))
 	 (uuid-second (substring raw-uuid 8 12))
	 (uuid-third  (substring raw-uuid 12 16))
	 (uuid-fourth (substring raw-uuid 16 20))
	 (uuid-fifth  (substring raw-uuid 20 32)))
    (downcase (format "%s-%s-%s-%s-%s"
	    uuid-first uuid-second uuid-third uuid-fourth uuid-fifth))))

(defun fa--find-uuid-start ()
  (+ (point) (skip-chars-backward "[[:alnum:]-]"))) 

(defun fa--find-uuid-end ()
  (+ (point) (skip-chars-forward "[[:alnum:]-]")))
