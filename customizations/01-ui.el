;;; UI --- Customizations. Mostly eliminating cruft.

;;; Commentary:

;;; Code:

;; Prevent the cursor from blinking
(blink-cursor-mode 0)

;; Don't use messages that you don't read
(setq initial-scratch-message nil
      inhibit-startup-message t
      inhibit-splash-screen 1
      initial-major-mode 'org-mode)


;; Don't let Emacs hurt your ears
(setq visible-bell t)
(setq ring-bell-function (lambda nil (message "")))

;; You need to set `inhibit-startup-echo-area-message' from the
;; customization interface:
;; M-x customize-variable RET inhibit-startup-echo-area-message RET
;; then enter your username
(setq inhibit-startup-echo-area-message "cclark")

;; Turn off UI widgets
(if (display-graphic-p)
    (progn
        (scroll-bar-mode -1)
        (tool-bar-mode -1)))

;; Modeline
(line-number-mode t)     ; Line numbers in mode line
(column-number-mode t)   ; Column numbers in mode line
(size-indication-mode 1) ; Display file size in k in modeline
(defvar linum-format)
(setq linum-format " %7d ")

; linum mode whenever we're in a programming mode
(add-hook 'prog-mode-hook 'linum-mode)

;; Highlights matching parenthesis
(show-paren-mode 1)

(when (>= emacs-major-version 24)
  ;; Uncomment if we need to look haxory for some reason
  ;;(load-theme 'cyberpunk t)
  (load-theme 'leuven t))

;; Highlight current line
(defvar hl-line-face)
(global-hl-line-mode 1)
(set-face-background hl-line-face "gray92")

(provide '01-ui)
;;; 01-ui.el ends here
