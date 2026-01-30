;;; fa-jwt.el -*- lexical-binding: t -*-

;;; Commentary

;;
;; Decode jwt-token
;;
;; jwt-token: eyJhbGciOiJIUzI1NiIsInR5cCI6IkpXVCJ9.eyJzdWIiOiIxMjM0NTY3ODkwIiwibmFtZSI6IkpvaG4gRG9lIiwiaWF0IjoxNTE2MjM5MDIyfQ.SflKxwRJSMeKKF2QT4fwpMeJf36POk6yJV_adQssw5c

(defun fa-jwt-decode ()
  "Decode jwt-token"
  (interactive)
  (let ((beginning (fa-jwt--find-token-start!))
	(end       (fa-jwt--find-token-end!)))
    (fa-jwt--decode-token-in-buffer! beginning end)))

(defun fa-jwt-decode-region (beginning end &optional all)
  "Decode jwt-token region"
  (interactive "r")
  (fa-jwt--decode-token-in-buffer! beginning end all))

;;
;; Pure functions (no side effects)
;;

(defun fa-jwt--decode (str all)
  (let* ((components (split-string str "\\."))
	 (header    (nth 0 components))
	 (payload   (nth 1 components))
	 (signature (nth 2 components)))
    (string-join
     (list
      (base64-decode-string header t)
      (base64-decode-string payload t)
      (if all
	  (base64-decode-string signature t)
	signature))
     ".")))

(defun fa-jwt--validate-jwt-token (str)
  "Validate that STR conforms with the aspect of a JWT token"
  (and (equal 3 (length (split-string str "\\.")))
       (string-match "[[:alnum:]=\\-_.]+" str)))

;;
;; Impure functions (with side effects)
;;

(defun fa-jwt--decode-token-in-buffer! (beginning end &optional all)
  "Decode jwt-token impl"
  (let ((str (buffer-substring-no-properties beginning end)))
    (unless (fa-jwt--validate-jwt-token str)
      (error "Couldn't recognize a JWT token"))
    ;; Side effects: Delete/insert in buffer
    (delete-region beginning end)
    (insert (fa-jwt--decode str all))))

(defun fa-jwt--find-token-start! ()
  ;; Side effect: Read/Move point
  (let ((p (point)))
    (+ (skip-chars-backward "[[:alnum:]=\\-_.]") p)))

(defun fa-jwt--find-token-end! ()
  ;; Side effect: Read/Move point
  (let ((p (point)))
    (+ p (skip-chars-forward "[[:alnum:]=\\-_.]"))))
