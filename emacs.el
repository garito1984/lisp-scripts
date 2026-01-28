;;
;; Emacs config file `.emacs`
;;
(require 'package)
(custom-set-variables
 ;; custom-set-variables was added by Custom.
 ;; If you edit it by hand, you could mess it up, so be careful.
 ;; Your init file should contain only one such instance.
 ;; If there is more than one, they won't work right.
 '(column-number-mode t)
 '(global-hl-line-mode t)
 '(custom-enabled-themes '(modus-vivendi))
 '(electric-pair-mode t)
 '(global-display-line-numbers-mode t)
 '(inhibit-startup-screen t)
 '(menu-bar-mode nil)
 '(package-archives
   '(("gnu" . "https://elpa.gnu.org/packages/")
     ("melpa" . "https://melpa.org/packages/")))
 '(package-selected-packages '(http rust-mode markdown-mode haskell-mode))
 '(safe-local-variable-values
   '((vc-prepare-patches-separately)
     (diff-add-log-use-relative-names . t)
     (vc-git-annotate-switches . "-w")))
 '(tool-bar-mode nil))
(package-initialize)
(custom-set-faces
 ;; custom-set-faces was added by Custom.
 ;; If you edit it by hand, you could mess it up, so be careful.
 ;; Your init file should contain only one such instance.
 ;; If there is more than one, they won't work right.
 )
(put 'upcase-region 'disabled nil)
(put 'downcase-region 'disabled nil)

(defun fa-scroll (lines)
  "Scroll text of selected window up or down LINES.

Scroll down if LINES is a negative number, scroll up otherwise."
  (interactive (let ((num (string-to-number (read-buffer "How many to scroll? "))))
                 (list num)))
  (if (< lines 0)
      (scroll-down (* -1 lines)) (scroll-up lines)))

(defun base64url-decode-region ()
  "FA function in .emacs to decode using base64url instead of the more standard base64 algo.

This is useful for instance to decode JWT tokens"
  (interactive)
  (base64-decode-region (region-beginning) (region-end) t))

;; Windows related config (where applicable)
;;(setenv "PATH" (concat "C:/Users/XXX/Downloads/mmmm/PortableGit/usr/bin;" (getenv "PATH")))

;; Load personal scripts
(load "~/Documents/lisp-scripts/fa-oracle-uuid")
(load "~/Documents/lisp-scripts/fa-jwt")

