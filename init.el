;;; Init -- Chris Clark's init.el file
;;; Commentary:
;;; Code:

(require 'cask "/usr/local/share/emacs/site-lisp/cask/cask.el")
(cask-initialize)

(require 'edit-server)
(setq edit-server-new-frame nil)
(edit-server-start)

;; Paths
(when (>= emacs-major-version 24)
  (require 'package)
  (package-initialize)
  (add-to-list 'package-archives '("melpa" . "http://melpa.milkbox.net/packages/") t)
  (add-to-list 'package-archives '("marmalade" . "http://marmalade-repo.org/packages/") t))

;; Load any extensions
(let ((default-directory (concat user-emacs-directory
                                 "ext")))
  (normal-top-level-add-to-load-path '("."))
  (normal-top-level-add-subdirs-to-load-path))

;; Put all backups in ~/.emacs.d/backups. More info:
(setq backup-directory-alist `(("." . ,(concat user-emacs-directory
                                               "backups"))))

(setq auto-save-default 1)
(setq auto-save-file-name-transforms `((".*" ,(concat user-emacs-directory
                                                      "backups") t)))
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
(mapc 'load (directory-files (concat user-emacs-directory
                                               "customizations") t "^[0-9]+.*\.el$"))

;;; init.el ends here
