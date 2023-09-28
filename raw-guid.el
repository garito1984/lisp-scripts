;;
;; Convert Oracle raw values into UUID and back
;;
;; UUID: 550e8400-e29b-41d4-a716-446655440000
;; RAW:      550E8400E29B41D4A716446655440000
;;
;; (load "~/Documents/lisp-scripts/raw-guid.el")

(defun fa-guid-convert-region ()
  "Convert value from RAW to UUID and vice versa"
  (interactive)
  (let* ((str (buffer-substring-no-properties (region-beginning) (region-end)))
	 (str-len (length str))
	 (converted-str str))
    (cond ((equal str-len 36) ; UUID
	   (setq converted-str (fa--guid-to-raw str)))
	  ((equal str-len 32) ; RAW
	   (setq converted-str (fa--raw-to-guid str)))
	  (t
	   (error "Not a valid RAW or plain UUID")))
    (delete-region (region-beginning) (region-end))
    (insert converted-str)))

(defun fa--guid-to-raw (guid)
  "Convert UUID into RAW"
  (string-replace "-" "" (upcase guid)))

(defun fa--raw-to-guid (raw-guid)
  "Convert RAW into a UUID"
  (let* ((uuid-first  (substring raw-guid 0 8))
 	 (uuid-second (substring raw-guid 8 12))
	 (uuid-third  (substring raw-guid 12 16))
	 (uuid-fourth (substring raw-guid 16 20))
	 (uuid-fifth  (substring raw-guid 20 32)))
    (downcase
     (string-join
      (list uuid-first
	    uuid-second
	    uuid-third
	    uuid-fourth
	    uuid-fifth)
      "-"))))

