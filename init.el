;;; Init -- Chris Clark's init.el file
;;; Commentary:
;;; Code:
(custom-set-variables
 ;; custom-set-variables was added by Custom.
 ;; If you edit it by hand, you could mess it up, so be careful.
 ;; Your init file should contain only one such instance.
 ;; If there is more than one, they won't work right.
 '(ansi-color-faces-vector
   [default bold shadow italic underline bold bold-italic bold])
 '(ansi-color-names-vector
   (vector "#708183" "#c60007" "#728a05" "#a57705" "#2075c7" "#c61b6e" "#259185" "#042028"))
 '(blink-cursor-mode nil)
 '(coffee-tab-width 2)
 '(column-number-mode t)
 '(custom-enabled-themes nil)
 '(custom-safe-themes
   (quote
    ("8aebf25556399b58091e533e455dd50a6a9cba958cc4ebb0aab175863c25b9a4" "4aee8551b53a43a883cb0b7f3255d6859d766b6c5e14bcb01bed572fcbef4328" default)))
 '(fringe-mode 6 nil (fringe))
 '(linum-format " %7d ")
 '(show-paren-mode t)
 '(size-indication-mode t)
 '(tool-bar-mode nil))

(require 'cask "~/.cask/cask.el")
(cask-initialize)

(require 'edit-server)
(setq edit-server-new-frame nil)
(edit-server-start)

;; Paths
(add-to-list 'exec-path' "/usr/local/bin/lein")
(add-to-list 'exec-path' "/usr/local/bin")

(add-to-list 'load-path' "~/.emacs.d/exts")

(autoload 'tads3-mode "tads3-mode" "TADS 2 editing mode." t)
  (setq auto-mode-alist
        (append (list (cons "\\.t$" 'tads2-mode))
                auto-mode-alist))

(when (>= emacs-major-version 24)
  (require 'package)
  (package-initialize)
  (add-to-list 'package-archives '("melpa" . "http://melpa.milkbox.net/packages/") t)
  (add-to-list 'package-archives '("marmalade" . "http://marmalade-repo.org/packages/") t)

  (defun install-if-needed (package)
    (unless (package-installed-p package)
      (package-install package))))

;; Auto-save and backup configuration
(setq backup-directory-alist `((".*"               . "~/.emacs-saves")))
(setq auto-save-file-name-transforms `((".*" ,"~/.emacs-saves" t)))
(setq make-backup-files t               ; backup of a file the first time it is saved.
      backup-by-copying t               ; don't clobber symlinks
      version-control t                 ; version numbers for backup files
      delete-old-versions t             ; delete excess backup files silently
      delete-by-moving-to-trash t
      kept-old-versions 6               ; oldest versions to keep when a new numbered backup is made (default: 2)
      kept-new-versions 9               ; newest versions to keep when a new numbered backup is made (default: 2)
      auto-save-default t               ; auto-save every buffer that visits a file
      auto-save-timeout 20              ; number of seconds idle time before auto-save (default: 30)
      auto-save-interval 200            ; number of keystrokes between auto-saves (default: 300)
      )

;; Entries in the ~/.emacs.d/customizations are of the form 02-global.el.
;; This allows control over initialization ordering
(mapc 'load (directory-files "~/.emacs.d/customizations" t "^[0-9]+.*\.el$"))

;;; init.el ends here
(put 'downcase-region 'disabled nil)
(custom-set-faces
 ;; custom-set-faces was added by Custom.
 ;; If you edit it by hand, you could mess it up, so be careful.
 ;; Your init file should contain only one such instance.
 ;; If there is more than one, they won't work right.
 '(company-scrollbar-bg ((t (:background "#005369"))))
 '(company-scrollbar-fg ((t (:background "#003f4f"))))
 '(company-tooltip ((t (:inherit default :background "#003340"))))
 '(company-tooltip-common ((t (:inherit font-lock-constant-face))))
 '(company-tooltip-selection ((t (:inherit font-lock-function-name-face)))))
