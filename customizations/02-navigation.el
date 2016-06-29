;;; package --- Navigation

;;; Commentary:
;; Moving around Emacs.  Primarily concerned with Helm
;; All helm keymappings are done in kbd.el

;;; Code:

;; "When several buffers visit identically-named files,
;; Emacs must give the buffers distinct names.  The usual method
;; for making buffer names unique adds ‘<2>’, ‘<3>’, etc to the end
;; of the buffer names (all but one of them).
;; The forward naming method includes part of the file's directory
;; name at the beginning of the buffer name
;; https://www.gnu.org/software/emacs/manual/html_node/emacs/Uniquify.html
(require 'uniquify)
(setq uniquify-buffer-name-style 'forward)

;;; Recentf:
(defvar recentf-save-file)
(setq recentf-save-file (concat user-emacs-directory ".recentf"))
(require 'recentf)
(recentf-mode 1)
(setq recentf-max-menu-items 200)

;;; Helm:
(require 'helm)
(require 'helm-config)
(require 'helm-descbinds)
(helm-descbinds-mode)

(defvar helm-M-x-fuzzy-match)
(defvar helm-buffers-fuzzy-matching)
(defvar helm-recentf-fuzzy-match)
(defvar helm-ff-search-library-in-sexp)
(defvar helm-ff-file-name-history-use-recentf)
(defvar helm-apropos-fuzzy-match)
(setq helm-M-x-fuzzy-match                  t
      helm-buffers-fuzzy-matching           t
      helm-recentf-fuzzy-match              t
      helm-split-window-in-side-p           t ; open helm buffer inside current window, not occupy whole other window
      helm-move-to-line-cycle-in-source     t ; move to end or beginning of source when reaching top or bottom of source.
      helm-ff-search-library-in-sexp        t ; search for library in `require' and `declare-function' sexp.
      helm-scroll-amount                    8 ; scroll 8 lines other window using M-<next>/M-<prior>
      helm-ff-file-name-history-use-recentf t
      helm-apropos-fuzzy-match              t)

(define-key helm-map (kbd "<tab>") 'helm-execute-persistent-action) ; rebind tab to run persistent action
(define-key helm-map (kbd "C-i")   'helm-execute-persistent-action) ; make TAB works in terminal
(define-key helm-map (kbd "C-z")   'helm-select-action)             ; list actions using C-z

(helm-mode 1)
(helm-flx-mode +1)

;;; Projectile:
(projectile-global-mode)
(defvar projectile-completion-system)
(setq projectile-completion-system 'helm)
(helm-projectile-on)

;;; IBuffer:
(add-hook 'ibuffer-hook
    (lambda ()
      (ibuffer-projectile-set-filter-groups)))

;; Speedbar/sr-speedbar
(defvar speedbar-show-unknown-files)
(setq speedbar-show-unknown-files t)

(require 'sr-speedbar)
(defvar speedbar-use-images)
(setq speedbar-use-images nil)
(sr-speedbar-refresh-turn-on)
(sr-speedbar-open)
(with-current-buffer sr-speedbar-buffer-name
  (setq window-size-fixed 'width))

(provide '02-navigation)
;;; 02-navigation.el ends here
