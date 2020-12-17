;;; clojure-mode --- set up clojure development environment

;;; Commentary:

;;; Code:

(add-to-list 'exec-path' "/usr/local/bin/lein")
(add-to-list 'exec-path' "/usr/local/bin")

;; A little more syntax highlighting
(require 'clojure-mode-extra-font-locking)

(add-hook 'cider-repl-mode-hook #'company-mode)
(add-hook 'cider-repl-mode-hook 'paredit-mode)

(add-hook 'clojure-mode-hook 'enable-paredit-mode)
(add-hook 'clojure-mode-hook #'rainbow-delimiters-mode)
(add-hook 'clojure-mode-hook #'subword-mode)
(add-hook 'clojure-mode-hook #'aggressive-indent-mode)

(add-hook 'cider-mode-hook #'company-mode)
(add-hook 'cider-mode-hook 'cider-turn-on-eldoc-mode)
(add-hook 'cider-mode-hook #'eldoc-mode)

;; Simple code folding
(add-hook 'clojure-mode-hook
  (lambda()
    (local-set-key (kbd "C-c <right>") 'hs-show-block)
    (local-set-key (kbd "C-c <left>")  'hs-hide-block)
    (local-set-key (kbd "C-c <up>")    'hs-hide-all)
    (local-set-key (kbd "C-c <down>")  'hs-show-all)
    (hs-minor-mode t)))

(require 'cider)
(setq cider-repl-pop-to-buffer-on-connect t
      cider-show-error-buffer t
      cider-auto-select-error-buffer nil
      cider-repl-history-file (concat user-emacs-directory
                                      "cider-history")
      cider-repl-wrap-history t
      nrepl-log-messages t)

;; Use clojure mode for other extensions
(add-to-list 'auto-mode-alist '("\\.edn$" . clojure-mode))
(add-to-list 'auto-mode-alist '("\\.boot$" . clojure-mode))
(add-to-list 'auto-mode-alist '("\\.cljs.*$" . clojure-mode))
(add-to-list 'auto-mode-alist '("lein-env" . enh-ruby-mode))

(provide '06-mode-clojure)
;;; 06-mode-clojure.el ends here
