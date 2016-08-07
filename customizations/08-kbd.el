;;; kbd --- all global keyboard remaps

;;; Commentary:
;; Mode-specific rebinds are handled in 04-modes.el.

;;; Code:

;; global shortcuts
(set-keyboard-coding-system nil)

;; Navigation
(global-set-key (kbd "C-s")     'isearch-forward-regexp)
(global-set-key (kbd "C-r")     'isearch-backward-regexp)
(global-set-key (kbd "C-M-s")   'isearch-forward)
(global-set-key (kbd "C-M-r")   'isearch-backward)
(global-set-key (kbd "C->")     'end-of-buffer)
(global-set-key (kbd "C-<")     'beginning-of-buffer)
(global-set-key (kbd "C-.")     'other-window)
(global-set-key (kbd "C-,")     (lambda () (interactive) (other-window -1)))
(global-set-key (kbd "C-x C-,") 'previous-buffer)
(global-set-key (kbd "C-x C-.") 'next-buffer)
(global-set-key (kbd "C-c s")   'sr-speedbar-toggle)
(global-set-key (kbd "M-p")     'backward-paragraph)
(global-set-key (kbd "M-n")     'forward-paragraph)

;; Killing
(global-set-key (kbd "C-w")     'backward-kill-word)
(global-set-key (kbd "C-x C-k") 'kill-region)

;; Editing
(global-set-key (kbd "C-x C-l") 'mc/edit-lines)
(global-set-key (kbd "C-x C-j") 'combine-line)
(global-set-key (kbd "C-=")     'er/expand-region)
(global-set-key (kbd "C-;")     'toggle-comment-on-line)
(global-set-key (kbd "C-x C-;") 'comment-dwim)
(global-set-key (kbd "C-c C--") 'hs-toggle-hiding)

;; Remaps -- mostly to helm
(global-set-key (kbd "C-x r l") 'helm-filtered-bookmarks)
(global-set-key (kbd "M-x")     'helm-M-x)
(global-set-key (kbd "C-x C-m") 'helm-M-x)
(global-set-key (kbd "C-x C-b") 'ibuffer)
(global-set-key (kbd "M-y")     'helm-show-kill-ring)
(global-set-key (kbd "C-x b")   'helm-mini)
(global-set-key (kbd "C-x C-f") 'helm-find-files)
(global-set-key (kbd "C-c g")   'helm-git-grep)
(global-set-key (kbd "C-c C-.") 'helm-git-grep-at-point)
(global-set-key (kbd "C-x C-d") 'helm-browse-project)
(global-set-key (kbd "C-x C-h") 'mark-whole-buffer)
(global-set-key (kbd "C-c h")   'helm-command-prefix)
(global-set-key (kbd "C-c h o") 'helm-occur)
(global-set-key (kbd "C-c c")   'helm-calcul-expression)

(global-set-key (kbd "C-h a")   'helm-apropos)
(global-set-key (kbd "C-x C-r") 'helm-recentf)

;; Misc
(define-key global-map [f5]     'toggle-truncate-lines)
(global-set-key (kbd "C-c d")   'insert-date)

(provide '08-kbd)
;;; 08-kbd.el ends here
