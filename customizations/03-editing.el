;;; 04-editing.el --- Customizations related to editing test

;;; Commentary:

;;; Code:
;; Key binding to use "hippie expand" for text autocompletion
;; http://www.emacswiki.org/emacs/HippieExpand
(global-set-key (kbd "M-/") 'hippie-expand)

;; Lisp-friendly hippie expand
(setq hippie-expand-try-functions-list
      '(try-expand-dabbrev
        try-expand-dabbrev-all-buffers
        try-expand-dabbrev-from-kill
        try-complete-lisp-symbol-partially
        try-complete-lisp-symbol))

;; Indentation
(setq-default tab-width 4)
(setq-default indent-tabs-mode nil)  ; always use spaces
(setq dtrt-indent-mode 1)            ; Adjust dtrt-indent-min-quality if needed
(electric-indent-mode +1)            ; This seems to be working well...


;; When you visit a file, point goes to the last place where it
;; was when you previously visited the same file.
;; http://www.emacswiki.org/emacs/SavePlace
(require 'saveplace)
(setq-default save-place t)
;; keep track of saved places in ~/.emacs.d/places
(setq save-place-file (concat user-emacs-directory "places"))

;; comments
(defun toggle-comment-on-line ()
  "Comment or uncomment current line."
  (interactive)
  (comment-or-uncomment-region (line-beginning-position) (line-end-position)))

;; fix weird os x kill errorn
(defun ns-get-pasteboard ()
  "Return the value of the pasteboard, or nil for unsupported formats."
  (condition-case nil
      (ns-get-selection-internal 'CLIPBOARD)
    (quit nil)))

;; expand-region
(require 'expand-region)

;; Globally turn on delete selection mode
(delete-selection-mode 1)

(provide '04-editing)
;;; 04-editing.el ends here
