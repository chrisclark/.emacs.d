;;; 04-modes.el --- Customizations related to editing test

;;; Commentary:

;;; Code:

;; markdown-mode
(autoload 'markdown-mode "markdown-mode"
   "Major mode for editing Markdown files" t)
(add-to-list 'auto-mode-alist '("\\.text\\'" . markdown-mode))
(add-to-list 'auto-mode-alist '("\\.markdown\\'" . markdown-mode))
(add-to-list 'auto-mode-alist '("\\.md\\'" . markdown-mode))


;; elisp-mode (and paredit)
(autoload 'enable-paredit-mode "paredit" "Turn on pseudo-structural editing of Lisp code." t)
(add-hook 'emacs-lisp-mode-hook       #'enable-paredit-mode)
(add-hook 'eval-expression-minibuffer-setup-hook #'enable-paredit-mode)
(add-hook 'ielm-mode-hook             #'enable-paredit-mode)
(add-hook 'lisp-mode-hook             #'enable-paredit-mode)
(add-hook 'lisp-interaction-mode-hook #'enable-paredit-mode)
(add-hook 'scheme-mode-hook           #'enable-paredit-mode)
(add-hook 'emacs-lisp-mode-hook 'turn-on-eldoc-mode)
(add-hook 'lisp-interaction-mode-hook 'turn-on-eldoc-mode)
(add-hook 'ielm-mode-hook 'turn-on-eldoc-mode)
(add-hook 'emacs-lisp-mode-hook (lambda () (flycheck-mode 1)))


;; Org-Mode
(require 'org)
(define-key global-map "\C-cl" 'org-store-link)
(define-key global-map "\C-ca" 'org-agenda)
(setq org-log-done t)
(setq org-startup-indented t)
(setq org-src-fontify-natively t)
(setq org-hide-emphasis-markers t)
(setq org-startup-with-inline-images t)
(defun my-org-screenshot ()
  "Take a screenshot with OS X.
Creates a time-stamped unique-named file in the same directory as
the org-buffer and insert a link to this file."
  (interactive)
  (org-display-inline-images)
  (defvar my-org-screenshot-filename)
  (setq my-org-screenshot-filename
        (concat
         (make-temp-name
          (concat (file-name-nondirectory (buffer-file-name))
                  "_imgs/"
                  (format-time-string "%Y%m%d_%H%M%S_")) ) ".png"))
  (unless (file-exists-p (file-name-directory my-org-screenshot-filename))
    (make-directory (file-name-directory my-org-screenshot-filename)))
  ; take screenshot
  (if (eq system-type 'darwin)
      (call-process "screencapture" nil nil nil "-i" my-org-screenshot-filename))
  (if (eq system-type 'gnu/linux)
      (call-process "import" nil nil nil my-org-screenshot-filename))
  ; insert into file if correctly taken
  (if (file-exists-p my-org-screenshot-filename)
    (insert (concat "[[file:" my-org-screenshot-filename "]]"))))


;; Git
(require 'magit)
(defvar magit-last-seen-setup-instructions)
(setq magit-last-seen-setup-instructions "1.4.0")
(global-set-key "\C-xg" 'magit-status)


;; Python
(require 'elpy)
(elpy-enable)
(when (require 'flycheck nil t)
  (setq elpy-modules (delq 'elpy-module-flymake elpy-modules))
  (add-hook 'elpy-mode-hook 'flycheck-mode))


;; Web stuff
(require 'tagedit)
(add-to-list 'auto-mode-alist '("\\.js$" . js-mode))
(add-to-list 'auto-mode-alist '("\\.ejs\\'" . html-mode))
(add-to-list 'auto-mode-alist '("\\.scss\\'" . css-mode))
(add-hook 'css-mode-hook 'rainbow-mode)
(add-hook 'js-mode-hook 'subword-mode)
(add-hook 'html-mode-hook 'subword-mode)
(defvar js-indent-level)
(setq js-indent-level 2)
(eval-after-load "sgml-mode"
  '(progn
     (require 'tagedit)
     (tagedit-add-paredit-like-keybindings)
     (add-hook 'html-mode-hook (lambda () (tagedit-mode 1)))))
(add-hook 'tagedit-mode-hook (tagedit-add-experimental-features))


;; YAML/LookML
(add-to-list 'auto-mode-alist '("\\.lookml\\'" . yaml-mode))


(provide '04-modes)
;;; 04-modes.el ends here
