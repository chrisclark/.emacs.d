;; global shortcuts
(set-keyboard-coding-system nil)

;; Navigation
(global-set-key (kbd "C-s") 'isearch-forward-regexp)
(global-set-key (kbd "C-r") 'isearch-backward-regexp)
(global-set-key (kbd "C-M-s") 'isearch-forward)
(global-set-key (kbd "C-M-r") 'isearch-backward)
(global-set-key (kbd "C->") 'end-of-buffer)
(global-set-key (kbd "C-<") 'beginning-of-buffer)

;; Killing
(global-set-key (kbd "C-w") 'backward-kill-word)
(global-set-key (kbd "C-x C-k") 'kill-region)

;; Editing
(global-set-key (kbd "C-x C-l") 'mc/edit-lines)
(global-set-key (kbd "C-x C-j") 'combine-line)
(global-set-key (kbd "C-=") 'er/expand-region)

;; Remaps -- mostly to helm
(global-set-key (kbd "C-x r l") 'helm-bookmarks)
(global-set-key (kbd "M-x") 'helm-M-x)
(global-set-key (kbd "C-x C-m") 'helm-M-x)
(global-set-key (kbd "C-x C-b") 'ibuffer)
(global-set-key (kbd "M-y") 'helm-show-kill-ring)
(global-set-key (kbd "C-x b") 'helm-mini)
(global-set-key (kbd "C-x C-f") 'helm-find-files)

(define-key global-map [f5] 'toggle-truncate-lines)
