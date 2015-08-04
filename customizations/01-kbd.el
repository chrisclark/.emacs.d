;; global shortcuts
(set-keyboard-coding-system nil)
(global-set-key "\C-x\C-m" 'smex)
(global-set-key "\C-w" 'backward-kill-word)
(global-set-key "\C-x\C-k" 'kill-region)
(global-set-key (kbd "C->") 'end-of-buffer)
(global-set-key (kbd "C-<") 'beginning-of-buffer)
(global-set-key (kbd "C-x C-l") 'mc/edit-lines)
(define-key global-map [f5] 'toggle-truncate-lines)
(global-set-key "\C-cy" '(lambda ()
   (interactive)
   (popup-menu 'yank-menu)))


