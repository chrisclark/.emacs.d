;;; navigation --- be fast!

;;; Commentary:
;; Moving around Emacs.  Primarily concerned with Helm.
;; All global helm keymappings are done in kbd.el

;;; Code:

;; Scroll one line at a time
(setq mouse-wheel-scroll-amount '(1 ((shift) . 1))
      scroll-step               1)

;; The forward naming method includes part of the file's directory
;; name at the beginning of the buffer name when sever buffers are visiting
;; identically-name files (instead of the default '<2>', '<3>', etc.
(require 'uniquify)
(setq uniquify-buffer-name-style 'post-forward)  ;; buffernames that are foo<1>, foo<2> -> foo|dir foo|otherdir

;;; Recentf:
(require 'recentf)
(recentf-mode 1)
(setq recentf-max-menu-items 200)

;;; Helm:
(require 'helm)
(require 'helm-config)
(require 'helm-descbinds)
(helm-descbinds-mode)

(setq helm-split-window-in-side-p           t ; open helm buffer inside current window, not occupy whole other window
      helm-move-to-line-cycle-in-source     t ; move to end or beginning of source when reaching top or bottom of source.
      helm-ff-search-library-in-sexp        t ; search for library in `require' and `declare-function' sexp.
      helm-scroll-amount                    8 ; scroll 8 lines other window using M-<next>/M-<prior>
      helm-ff-file-name-history-use-recentf t
      helm-recentf-fuzzy-match              t
      helm-M-x-fuzzy-match                  t
      helm-buffers-fuzzy-matching           t
      helm-apropos-fuzzy-match              t)

(define-key helm-map (kbd "<tab>") 'helm-execute-persistent-action) ; rebind tab to run persistent action
(define-key helm-map (kbd "C-i")   'helm-execute-persistent-action) ; make TAB works in terminal
(define-key helm-map (kbd "C-z")   'helm-select-action)             ; list actions using C-z

(helm-mode 1)
(helm-flx-mode +1)

;;; Projectile:
(require 'projectile)
(projectile-global-mode)
(setq projectile-completion-system 'helm)
(helm-projectile-on)

;;; Company:
(require 'company)
(global-company-mode 1)
(setq company-idle-delay              0.1
      company-tooltip-limit           10
      company-minimum-prefix-length   2
      company-tooltip-flip-when-above t)

;;; IBuffer:
(add-hook 'ibuffer-hook
    (lambda ()
      (ibuffer-projectile-set-filter-groups)))

;; Speedbar/sr-speedbar
(require 'sr-speedbar)
(setq speedbar-show-unknown-files       t
      speedbar-directory-unshown-regexp "^\\(CVS\\|RCS\\|SCCS\\|\\.\\.*$\\)\\'"  ;; Show hidden files, eg. dot files
      sr-speedbar-right-side            nil
      speedbar-use-images               nil)

(sr-speedbar-refresh-turn-on)

(require 'sr-speedbar)
(add-hook 'sr-speedbar-mode
	  (defun size-speedbar-buffer()
	    (with-current-buffer sr-speedbar-buffer-name
	      (setq window-size-fixed 'width))))

(provide '02-navigation)
;;; 02-navigation.el ends here
