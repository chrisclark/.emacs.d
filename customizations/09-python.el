;; Python stuff
(setq-default indent-tabs-mode nil)    ; use only spaces and no tabs
(setq default-tab-width 4)

;; make more packages available with the package installer
(setq to-install
      '(python-mode magit yasnippet jedi auto-complete autopair find-file-in-repository flycheck))

(mapc 'install-if-needed to-install)

(setq magit-last-seen-setup-instructions "1.4.0")
(require 'magit)
(global-set-key "\C-xg" 'magit-status)


(require 'auto-complete)
(require 'autopair)
(require 'yasnippet)
(require 'flycheck)
(global-flycheck-mode t)

(global-set-key [f7] 'find-file-in-repository)

; auto-complete mode extra settings
(setq
 ac-auto-start 2
 ac-override-local-map nil
 ac-use-menu-map t
 ac-candidate-limit 20)

;; ;; Python mode settings
(require 'python-mode)
(add-to-list 'auto-mode-alist '("\\.py$" . python-mode))
(setq py-electric-colon-active t)
(add-hook 'python-mode-hook 'autopair-mode)
(add-hook 'python-mode-hook 'yas-minor-mode)

;; ;; Jedi settings
(require 'jedi)
;; It's also required to run "pip install --user jedi" and "pip
;; install --user epc" to get the Python side of the library work
;; correctly.
;; With the same interpreter you're using.

;; if you need to change your python intepreter, if you want to change it
;; (setq jedi:server-command
;;       '("python2" "/home/andrea/.emacs.d/elpa/jedi-0.1.2/jediepcserver.py"))

(add-hook 'python-mode-hook
    (lambda ()
      (jedi:setup)
      (jedi:ac-setup)
            (local-set-key "\C-cd" 'jedi:show-doc)
            (local-set-key (kbd "M-SPC") 'jedi:complete)
            (local-set-key (kbd "M-.") 'jedi:goto-definition)))

(setq jedi:complete-on-dot t)
(add-hook 'python-mode-hook 'auto-complete-mode)
