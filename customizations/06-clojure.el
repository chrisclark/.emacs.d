;;; clojure-mode --- set up clojure development environment

;;; Commentary:

;;; Code:

(add-to-list 'exec-path' "/usr/local/bin/lein")
(add-to-list 'exec-path' "/usr/local/bin")

(require 'rainbow-delimiters)
(add-hook 'clojure-mode-hook 'enable-paredit-mode)
(add-hook 'clojure-mode-hook #'rainbow-delimiters-mode)

;; This is useful for working with camel-case tokens, like names of
;; Java classes (e.g. JavaClassName)
(add-hook 'clojure-mode-hook 'subword-mode)

;; A little more syntax highlighting
(require 'clojure-mode-extra-font-locking)


;; Cider

;; enable auto-completion & paredit inside of source code and repl
(add-hook 'cider-repl-mode-hook #'company-mode)
(add-hook 'cider-mode-hook #'company-mode)
(add-hook 'cider-repl-mode-hook 'paredit-mode)

;; minibuffer documentation in the repl
(add-hook 'cider-mode-hook 'cider-turn-on-eldoc-mode)
(add-hook 'cider-mode-hook #'eldoc-mode)

;; go right to the REPL buffer when it's finished connecting
(defvar cider-repl-pop-to-buffer-on-connect)
(defvar nrepl-log-messages)
(setq cider-repl-pop-to-buffer-on-connect t)
(setq nrepl-log-messages t)

;; Simple code folding
(add-hook 'clojure-mode-hook
  (lambda()
    (local-set-key (kbd "C-c <right>") 'hs-show-block)
    (local-set-key (kbd "C-c <left>")  'hs-hide-block)
    (local-set-key (kbd "C-c <up>")    'hs-hide-all)
    (local-set-key (kbd "C-c <down>")  'hs-show-all)
    (hs-minor-mode t)))

;; When there's a cider error, show its buffer but dont switch to it
(defvar cider-show-error-buffer)
(setq cider-show-error-buffer t)
(defvar cider-auto-select-error-buffer)
(setq cider-auto-select-error-buffer nil)

;; Where to store the cider history.
(defvar cider-repl-history-file)
(setq cider-repl-history-file (concat user-emacs-directory
                                               "cider-history"))

;; Wrap when navigating history.
(defvar cider-repl-wrap-history)
(setq cider-repl-wrap-history t)

;; enable paredit in your REPL


;; Use clojure mode for other extensions
(add-to-list 'auto-mode-alist '("\\.edn$" . clojure-mode))
(add-to-list 'auto-mode-alist '("\\.boot$" . clojure-mode))
(add-to-list 'auto-mode-alist '("\\.cljs.*$" . clojure-mode))
(add-to-list 'auto-mode-alist '("lein-env" . enh-ruby-mode))

(provide '07-mode-clojure)
;;; 07-mode-clojure.el ends here
