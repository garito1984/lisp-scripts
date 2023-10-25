;;
;; Convert Oracle raw values into UUID and back
;;
;; UUID: 550e8400-e29b-41d4-a716-446655440000
;; RAW:      550E8400E29B41D4A716446655440000
;;
;; (load "~/Documents/lisp-scripts/raw-guid.el")

(defun fa-guid-convert ()
  "Convert raw/guid value"
  (interactive)
  (fa--guid-convert-region-impl (fa--find-guid-start) (fa--find-guid-end)))

(defun fa-guid-convert-region ()
  "Convert value from RAW to UUID and vice versa"
  (interactive)
  (fa--guid-convert-region-impl (region-beginning) (region-end)))

(defun fa--guid-convert-region-impl (beginning end)
  "Convert value from RAW to UUID and vice versa"
  (interactive)
  (let* ((str (buffer-substring-no-properties beginning end))
	 (str-len (length str))
	 (converted-str str))
    (cond ((equal str-len 36) ; UUID
	   (setq converted-str (fa--guid-to-raw str)))
	  ((equal str-len 32) ; RAW
	   (setq converted-str (fa--raw-to-guid str)))
	  (t
	   (error "Not a valid RAW or plain UUID")))
    (delete-region beginning end)
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

(defun fa--find-guid-start ()
  (re-search-backward "\\(^[[:graph:]]\\|[^[:graph:]]\\)") ;; Find first non graph char or the first char if token is at the beginning of the line
  (re-search-forward "[[:blank:][:cntrl:]]*")) ;; If there is a blank or newline char, skip it

(defun fa--find-guid-end ()
  (re-search-forward "[[:graph:]]+"))
