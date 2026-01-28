;;; fa-jwt.el -*- lexical-binding: t -*-

;;; Commentary

;;
;; Decode jwt-token
;;
;; jwt-token: eyJhbGciOiJIUzI1NiIsInR5cCI6IkpXVCJ9.eyJzdWIiOiIxMjM0NTY3ODkwIiwibmFtZSI6IkpvaG4gRG9lIiwiaWF0IjoxNTE2MjM5MDIyfQ.SflKxwRJSMeKKF2QT4fwpMeJf36POk6yJV_adQssw5c

(defun fa-jwt-decode ()
  "Decode jwt-token"
  (interactive)
  (fa-jwt--decode-impl! (fa-jwt--find-token-start!) (fa-jwt--find-token-end!)))

(defun fa-jwt-decode-region (beginning end &optional all)
  "Decode jwt-token region"
  (interactive "r")
  (fa-jwt--decode-impl! beginning end all))

;;
;; Pure functions (no side effects)
;;

;;
;; Impure functions (with side effects)
;;

(defun fa-jwt--decode-impl! (beginning end &optional all)
  "Decode jwt-token impl"
  (let* ((components (fa-jwt--buffer-token-subcomponents! beginning end))
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

(defun fa-jwt--buffer-token-subcomponents! (beginning end)
  (split-string (buffer-substring-no-properties beginning end) "\\."))

(defun fa-jwt--find-token-start! ()
  (let ((p (point)))
    (+ p (skip-chars-backward "[[:alnum:]_.]"))))

(defun fa-jwt--find-token-end! ()
  (let ((p (point)))
    (+ p (skip-chars-forward "[[:alnum:]_.]"))))
