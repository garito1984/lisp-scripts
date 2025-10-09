;;; oracle-uuid.el -*- lexical-binding: t -*-

;;; Commentary:

;;
;; Convert Oracle raw values into UUID and back
;;
;; UUID: 550e8400-e29b-41d4-a716-446655440000
;; RAW:      550E8400E29B41D4A716446655440000
;;
;; (load "~/Documents/lisp-scripts/oracle-uuid.el")

;;; Code:

(defun fa-oracle-uuid-convert ()
  "Convert value from RAW to UUID and vice versa"
  (interactive)
  (let ((begin (oracle--find-uuid-start!))
	(end   (oracle--find-uuid-end!)))
    (oracle--update-buffer! begin end)))

(defun fa-oracle-uuid-convert-region ()
  "Convert value from RAW to UUID and vice versa"
  (interactive)
  (let ((begin (region-beginning))
	(end   (region-end)))
    (oracle--update-buffer! begin end)))

;;
;; Pure functions (no side effects)
;;

(defun oracle--uuid-convert (str)
  "Convert value from RAW to UUID and vice versa"
  (let ((str-len (length str)))
    (cond ((equal str-len 36) ; UUID
	   (oracle--uuid-to-raw str))
	  ((equal str-len 32) ; RAW
	   (oracle--raw-to-uuid str))
	  (t
	   nil))))

(defun oracle--uuid-to-raw (uuid)
  "Convert UUID into RAW"
  (upcase (string-replace "-" "" uuid)))

(defun oracle--raw-to-uuid (raw-uuid)
  "Convert RAW into a UUID"
  (let ((sections '((0 . 8) (8 . 12) (12 . 16) (16 . 20) (20 . 32)))
	(ext-sect (lambda (s) (substring raw-uuid (car s) (cdr s)))))
    (downcase (apply 'format "%s-%s-%s-%s-%s" (mapcar ext-sect sections)))))

;;
;; Impure functions (with side effects)
;;

(defun oracle--update-buffer! (begin end)
  "Convert raw/uuid value"
  (let ((uuid (oracle--uuid-convert (buffer-substring-no-properties begin end))))
    (if (null uuid)
	(error "Not a valid RAW or plain UUID"))
    (delete-region begin end)
    (insert uuid)))

(defun oracle--find-uuid-start! ()
  (+ (point) (skip-chars-backward "[[:alnum:]-]")))

(defun oracle--find-uuid-end! ()
  (+ (point) (skip-chars-forward "[[:alnum:]-]")))
