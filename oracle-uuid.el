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
  (oracle--uuid-convert-region-impl (oracle--find-uuid-start) (oracle--find-uuid-end)))

(defun fa-oracle-uuid-convert-region ()
  "Convert value from RAW to UUID and vice versa"
  (interactive)
  (oracle--uuid-convert-region-impl (region-beginning) (region-end)))

(defun oracle--uuid-convert-region-impl (beginning end)
  "Convert value from RAW to UUID and vice versa"
  (let* ((str (buffer-substring-no-properties beginning end))
	 (str-len (length str))
	 (converted-str str))
    (cond ((equal str-len 36) ; UUID
	   (setq converted-str (oracle--uuid-to-raw str)))
	  ((equal str-len 32) ; RAW
	   (setq converted-str (oracle--raw-to-uuid str)))
	  (t
	   (error "Not a valid RAW or plain UUID")))
    (delete-region beginning end)
    (insert converted-str)))

(defun oracle--uuid-to-raw (uuid)
  "Convert UUID into RAW"
  (upcase (string-replace "-" "" uuid)))

(defun oracle--raw-to-uuid (raw-uuid)
  "Convert RAW into a UUID"
  (let* ((uuid-first  (substring raw-uuid 0 8))
 	 (uuid-second (substring raw-uuid 8 12))
	 (uuid-third  (substring raw-uuid 12 16))
	 (uuid-fourth (substring raw-uuid 16 20))
	 (uuid-fifth  (substring raw-uuid 20 32)))
    (downcase (format "%s-%s-%s-%s-%s"
	    uuid-first uuid-second uuid-third uuid-fourth uuid-fifth))))

(defun oracle--find-uuid-start ()
  (+ (point) (skip-chars-backward "[[:alnum:]-]"))) 

(defun oracle--find-uuid-end ()
  (+ (point) (skip-chars-forward "[[:alnum:]-]")))
