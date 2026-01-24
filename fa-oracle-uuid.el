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
  "Convert UUID from RAW to DASH and vice versa"
  (interactive)
  (let ((begin (fa-oracle--find-uuid-start!))
	(end   (fa-oracle--find-uuid-end!)))
    (fa-oracle--update-buffer! begin end)))

(defun fa-oracle-uuid-convert-region ()
  "Convert UUID from RAW to DASH format and vice versa"
  (interactive)
  (let ((begin (region-beginning))
	(end   (region-end)))
    (fa-oracle--update-buffer! begin end)))

;;
;; Pure functions (no side effects)
;;

(defun fa-oracle--uuid-convert (str)
  "Convert UUID from RAW to DASH format and vice versa"
  (pcase (length str) 
    (36 (fa-oracle--uuid-dash-to-raw str)) ; DASH
    (32 (fa-oracle--uuid-raw-to-dash str)) ; RAW
    (_ nil)))

(defun fa-oracle--uuid-dash-to-raw (dash-uuid)
  "Convert DASH format UUID into RAW"
  (upcase (string-replace "-" "" dash-uuid)))

(defun fa-oracle--uuid-raw-to-dash (raw-uuid)
  "Convert RAW format UUID into DASH"
  (let ((components
	 (named-let extract ((from 0) (tos '(8 12 16 20 32)))
	   (if tos
	       (cons (substring raw-uuid from (car tos))
		     (extract (car tos) (cdr tos)))))))
    (downcase (string-join components "-"))))

;;
;; Impure functions (with side effects)
;;

(defun fa-oracle--update-buffer! (begin end)
  "Convert uuid into raw/dash format"
  (let ((uuid (fa-oracle--uuid-convert (buffer-substring-no-properties begin end)))) ; Side effect: Read buffer
    (unless uuid
      (error "Not a valid RAW or DASH UUID"))
    ;; Side effects: Delete/insert in buffer
    (delete-region begin end)
    (insert uuid)))

(defun fa-oracle--find-uuid-start! ()
  ;; Side effect: Move point
  (+ (point) (skip-chars-backward "[[:alnum:]-]")))

(defun fa-oracle--find-uuid-end! ()
  ;; Side effect: Move point
  (+ (point) (skip-chars-forward "[[:alnum:]-]")))
