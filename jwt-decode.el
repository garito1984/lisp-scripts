;;
;; Decode jwt-token
;;
;; jwt-token: eyJhbGciOiJIUzI1NiIsInR5cCI6IkpXVCJ9.eyJzdWIiOiIxMjM0NTY3ODkwIiwibmFtZSI6IkpvaG4gRG9lIiwiaWF0IjoxNTE2MjM5MDIyfQ.SflKxwRJSMeKKF2QT4fwpMeJf36POk6yJV_adQssw5c

(defun fa-jwt-decode-region (&optional all)
  "Decode jwt-token"
  (interactive)
  (let* ((token (buffer-substring-no-properties (region-beginning) (region-end)))
	 (tokens (split-string token "\\."))
	 (header (car tokens))
	 (payload (cadr tokens))
	 (signature (caddr tokens)))
    (delete-region (region-beginning) (region-end))
    (insert
     (string-join
      (list
       (base64-decode-string header t)
       (base64-decode-string payload t)
       (if all
	   (base64-decode-string signature t)
	 signature))
      "."))))
