;;
;; Decode jwt-token
;;
;; jwt-token: eyJhbGciOiJIUzI1NiIsInR5cCI6IkpXVCJ9.eyJzdWIiOiIxMjM0NTY3ODkwIiwibmFtZSI6IkpvaG4gRG9lIiwiaWF0IjoxNTE2MjM5MDIyfQ.SflKxwRJSMeKKF2QT4fwpMeJf36POk6yJV_adQssw5c

(defun fa-jwt-decode ()
  "Decode jwt-token"
  (interactive)
  (fa--jwt-decode-impl (fa--find-jwt-token-start) (fa--find-jwt-token-end)))

(defun fa-jwt-decode-region (&optional all)
  "Decode jwt-token region"
  (interactive)
  (fa--jwt-decode-impl (region-beginning) (region-end) all))

(defun fa--jwt-decode-impl (beginning end &optional all)
  "Decode jwt-token impl"
  (let* ((components (fa--buffer-jwt-token-subcomponents beginning end))
	 (header (pop components))
	 (payload (pop components))
	 (signature (pop components)))
    (delete-region beginning end)
    (insert
     (string-join
      (list
       (base64-decode-string header t)
       (base64-decode-string payload t)
       (if all
	   (base64-decode-string signature t)
	 signature))
      "."))))

(defun fa--buffer-jwt-token-subcomponents (beginning end)
  (split-string (buffer-substring-no-properties beginning end) "\\."))

(defun fa--find-jwt-token-start ()
  (re-search-backward "\\(^[[:graph:]]\\|[^[:graph:]]\\)") ;; Find first non graph char or the first char if token is at the beginning of the line
  (re-search-forward "[[:blank:][:cntrl:]]*")) ;; If there is a blank or newline char, skip it

(defun fa--find-jwt-token-end ()
  (re-search-forward "[[:graph:]]+"))
