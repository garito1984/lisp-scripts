;;
;; Decode jwt-token
;;
;; jwt-token: eyJhbGciOiJIUzI1NiIsInR5cCI6IkpXVCJ9.eyJzdWIiOiIxMjM0NTY3ODkwIiwibmFtZSI6IkpvaG4gRG9lIiwiaWF0IjoxNTE2MjM5MDIyfQ.SflKxwRJSMeKKF2QT4fwpMeJf36POk6yJV_adQssw5c

(defun fa-jwt-decode ()
  "Decode jwt-token"
  (interactive)
  (fa--jwt-decode-impl (re-search-backward "[^[:graph:]]") (re-search-forward "[[:graph:]]+")))

(defun fa-jwt-decode-region (&optional all)
  "Decode jwt-token region"
  (interactive)
  (fa--jwt-decode-impl (region-beginning) (region-end) all))

(defun fa--jwt-decode-impl (beginning end &optional all)
  "Decode jwt-token impl"
  (let* ((token (buffer-substring-no-properties beginning end))
	 (tokens (split-string token "\\."))
	 (header (car tokens))
	 (payload (cadr tokens))
	 (signature (caddr tokens)))
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


