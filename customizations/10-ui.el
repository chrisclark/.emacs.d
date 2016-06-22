;; Prevent the cursor from blinking
(blink-cursor-mode 0)

;; Don't use messages that you don't read
(setq initial-scratch-message "")
(setq inhibit-startup-message t)

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


(line-number-mode t)
(column-number-mode t)
(size-indication-mode t)

(when (>= emacs-major-version 24)
  ;;(load-theme 'solarized-dark)
  ;;(load-theme 'leuven t)
  (load-theme 'cyberpunk t))

(custom-set-faces
 '(company-scrollbar-bg ((t (:background "#005369"))))
 '(company-scrollbar-fg ((t (:background "#003f4f"))))
 '(company-tooltip ((t (:inherit default :background "#003340"))))
 '(company-tooltip-common ((t (:inherit font-lock-constant-face))))
 '(company-tooltip-selection ((t (:inherit font-lock-function-name-face)))))

