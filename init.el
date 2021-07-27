;;; Init -- Chris Clark's init.el file
;;; Commentary:
;;; Code:

(setq warning-suppress-log-types '((package reinitialization)))

(require 'cask)
(cask-initialize)

;(require 'edit-server)
;(setq edit-server-new-frame nil)
;(edit-server-start)

;; Paths
(when (>= emacs-major-version 24)
  (require 'package)
  ;(package-initialize)
  (add-to-list 'package-archives '("melpa" . "https://melpa.org/packages/") t)
)

;; Load any extensions
(let ((default-directory (concat user-emacs-directory "ext")))
(normal-top-level-add-to-load-path '("."))
(normal-top-level-add-subdirs-to-load-path))

(setq make-backup-files         t    ; backup of a file the first time it is saved.
      backup-by-copying         t    ; don't clobber symlinks
      version-control           t    ; version numbers for backup files
      delete-old-versions       t    ; delete excess backup files silently
      kept-old-versions         6    ; oldest versions to keep when a new numbered backup is made (default: 2)
      kept-new-versions         9    ; newest versions to keep when a new numbered backup is made (default: 2)
      auto-save-default         t    ; auto-save every buffer that visits a file
      auto-save-timeout         20   ; number of seconds idle time before auto-save (default: 30)
      auto-save-interval        200  ; number of keystrokes between auto-saves (default: 300)
      delete-by-moving-to-trash t)

;; Put all backups and auto-saves in ~/.emacs.d/backups.
(setq backup-directory-alist
      `((".*" . ,(concat user-emacs-directory "backups")))
      auto-save-file-name-transforms
      `((".*" ,(concat user-emacs-directory "backups") t)))

;; Entries in the ~/.emacs.d/customizations are of the form 02-global.el.
;; This allows control over initialization ordering
(mapc 'load (directory-files (concat user-emacs-directory
                                               "customizations") t "^[0-9]+.*\.el$"))

(set-language-environment "UTF-8")
(set-default-coding-systems 'utf-8)

;;; init.el ends here
(custom-set-variables
 ;; custom-set-variables was added by Custom.
 ;; If you edit it by hand, you could mess it up, so be careful.
 ;; Your init file should contain only one such instance.
 ;; If there is more than one, they won't work right.
 '(package-selected-packages
   '(treemacs-projectile treemacs typescript-mode impatient-mode crux vue-mode polymode ess indium js2-refactor xref-js2 js2-mode company-tern web-mode use-package tidy tagedit sr-speedbar sqlup-mode smex smartparens session restclient rainbow-mode rainbow-delimiters prodigy popwin paredit multiple-cursors markdown-mode magit leuven-theme json-reformat jedi idle-highlight-mode ibuffer-projectile htmlize helm-projectile helm-ls-git helm-git-grep helm-flx helm-descbinds git-gutter flycheck-cask expand-region exec-path-from-shell elpy edit-server dtrt-indent drag-stuff clojure-mode-extra-font-locking cider centered-cursor-mode cask browse-kill-ring autopair))
 '(session-use-package t nil (session)))
(custom-set-faces
 ;; custom-set-faces was added by Custom.
 ;; If you edit it by hand, you could mess it up, so be careful.
 ;; Your init file should contain only one such instance.
 ;; If there is more than one, they won't work right.
 '(fringe ((t (:background "white")))))
