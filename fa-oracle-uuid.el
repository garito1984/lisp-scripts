;;; oracle-uuid.el -*- lexical-binding: t -*-

;;; Commentary:

;;
;; Convert Oracle UUID between the DASH or RAW format
;;
;; DASH: 550e8400-e29b-41d4-a716-446655440000
;;  RAW: 550E8400E29B41D4A716446655440000
;;
;; (load "~/Documents/lisp-scripts/fa-oracle-uuid")

;;; Code:

(defun fa-oracle-uuid-convert ()
  "Convert value from RAW to UUID and vice versa"
  (interactive)
  (let ((begin (fa-oracle--find-uuid-start!))
	(end   (fa-oracle--find-uuid-end!)))
    (fa-oracle--update-buffer! begin end)))

(defun fa-oracle-uuid-convert-region ()
  "Convert value from RAW to UUID and vice versa"
  (interactive)
  (let ((begin (region-beginning))
	(end   (region-end)))
    (fa-oracle--update-buffer! begin end)))

;;
;; Pure functions (no side effects)
;;

(defun fa-oracle--uuid-convert (str)
  "Convert value from RAW to UUID and vice versa"
  (let ((len (length str)))
    (cond ((equal len 36) ; UUID
	   (fa-oracle--uuid-dash-to-raw str))
	  ((equal len 32) ; RAW
	   (fa-oracle--uuid-raw-to-dash str))
	  (t nil))))

(defun fa-oracle--uuid-dash-to-raw (uuid)
  "Convert UUID into RAW"
  (upcase (string-replace "-" "" uuid)))

(defun fa-oracle--uuid-raw-to-dash (raw-uuid)
  "Convert RAW into a UUID"
  (let ((sections '((0 . 8) (8 . 12) (12 . 16) (16 . 20) (20 . 32)))
	(ext-sect (lambda (s) (substring raw-uuid (car s) (cdr s)))))
    (downcase (apply 'format "%s-%s-%s-%s-%s" (mapcar ext-sect sections)))))

;;
;; Impure functions (with side effects)
;;

(defun fa-oracle--update-buffer! (begin end)
  "Convert raw/uuid value"
  (let ((uuid (fa-oracle--uuid-convert (buffer-substring-no-properties begin end))))
    (unless uuid
      (error "Not a valid RAW or plain UUID"))
    (delete-region begin end)
    (insert uuid)))

(defun fa-oracle--find-uuid-start! ()
  (+ (point) (skip-chars-backward "[[:alnum:]-]")))

(defun fa-oracle--find-uuid-end! ()
  (+ (point) (skip-chars-forward "[[:alnum:]-]")))
