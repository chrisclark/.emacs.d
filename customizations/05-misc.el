;;; misc --- odds and ends and custom functions

;;; Commentary:

;;; Code:

;; Bookmarks under source control
(defvar bookmark-version-control)
(setq bookmark-version-control t)

;; Inexplicably, this command is disabled by default. That's silly.
(put 'downcase-region 'disabled nil)

;; Changes all yes/no questions to y/n type
(fset 'yes-or-no-p 'y-or-n-p)

;; Save M-x history between sessions
(add-hook 'after-init-hook 'session-initialize)

;; shell scripts
(setq-default sh-basic-offset 2)
(setq-default sh-indentation 2)

;; No need for ~ files when editing
(setq create-lockfiles nil)

;; Go straight to scratch buffer on startup
(setq inhibit-startup-message t)

(add-hook 'tagedit-mode-hook (tagedit-add-experimental-features))

(global-git-gutter-mode +1)

;;; Custom Functions
(defun combine-line ()
  "Joins the line to the line above."
  (interactive)
  (move-beginning-of-line 1)
  (delete-horizontal-space)
  (backward-delete-char 1)
  (insert " "))

; Slightly modified from http://stackoverflow.com/questions/17922208/emacs-convert-items-on-separate-lines-to-a-comma-separated-list
(defun arrayify (start end &optional arg)
  "Turns a series of strings on newlines into single quoted, comma separated one-liner."
  (interactive "r\nP")
  (let ((insertion
         (mapconcat 
          (lambda (x) (format "'%s'" x))
          (split-string (buffer-substring start end)) ", ")))
    (delete-region start end)
    (insert insertion)
    (when arg (forward-char (length insertion)))))

(defun arrayify-unquoted (start end &optional arg)
  "Turns a series of strings on newlines into single quoted, comma separated one-liner."
  (interactive "r\nP")
  (let ((insertion
         (mapconcat 
          (lambda (x) (format "%s" x))
          (split-string (buffer-substring start end)) ", ")))
    (delete-region start end)
    (insert insertion)
    (when arg (forward-char (length insertion)))))


(defun insert-date (prefix)
    "Insert the current date.  With prefix-argument, use ISO format.
   With two prefix arguments, write out the day and month name."
    (interactive "P")
    (let ((format (cond
                   ((not prefix) "%Y-%m-%d")
                   ((equal prefix '(4)) "%d.%m.%Y")
                   ((equal prefix '(16)) "%A, %d. %B %Y")))
          (system-time-locale "de_DE"))
      (insert (format-time-string format))))

(defun my-tag-lines (b e tag)
  "HTML: Wrap every line in the region with a tag."
  (interactive "r\nMTag for line: ")
  (save-restriction
    (narrow-to-region b e)
    (save-excursion
      (goto-char (point-min))
      (while (< (point) (point-max))
        (beginning-of-line)
        (insert (format "<%s>" tag))
        (end-of-line)
        (insert (format "</%s>" tag))
        (forward-line 1)))))

(defvar sql-buffer)
(defun sql-setup-postgres ()
  "Set up a postgres connection and sqli mode."
  (interactive)
  (sql-postgres)
  (sql-set-product "postgres")
  (setq sql-buffer "*SQL*")
  (run-hooks 'sql-set-sqli-hook)
  (sql-mode))

;; Fix foolish calendar-mode scrolling.
;; From https://sites.google.com/site/steveyegge2/my-dot-emacs-file
(add-hook 'calendar-load-hook
 '(lambda ()
 (setq mark-holidays-in-calendar t)
 (define-key calendar-mode-map ">" 'scroll-calendar-left)
 (define-key calendar-mode-map "<" 'scroll-calendar-right)))

(provide '06-misc)
;;; 06-misc.el ends here
