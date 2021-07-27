;;; misc --- odds and ends and custom functions

;;; Commentary:

;;; Code:

;; Bookmarks under source control
(defvar bookmark-version-control)
(setq bookmark-version-control t)

;; Inexplicably, this command is disabled by default. That's silly.
(put 'downcase-region 'disabled nil)

;; If a file has changed on disk, automaticaly revert the buffer
(global-auto-revert-mode 1)

;; Changes all yes/no questions to y/n type
(fset 'yes-or-no-p 'y-or-n-p)

;; Save M-x history between sessions
(add-hook 'after-init-hook 'session-initialize)

;; No need for ~ files when editing
(setq create-lockfiles nil)

;;; Custom Functions
(defun combine-line ()
  "Joins the line to the line above."
  (interactive)
  (move-beginning-of-line 1)
  (delete-horizontal-space)
  (backward-delete-char 1)
  (insert " "))

;; Slightly modified from
;; http://stackoverflow.com/questions/17922208/emacs-convert-items-on-separate-lines-to-a-comma-separated-list
;;;###autoload
(defun arrayify (start end quote)
  "Turn newlines between START and END into a QUOTE d, comma-separated one-liner."
  (interactive "r\nMQuote: ")
  (let ((insertion
         (mapconcat
          (lambda (x) (format "%s%s%s" quote x quote))
          (split-string (buffer-substring start end)) ", ")))
    (delete-region start end)
    (insert insertion)))

(defun insert-date (prefix)
    "Insert the current date.  With PREFIX, use ISO format.
With two prefix arguments, write out the day and month name."
    (interactive "P")
    (let ((format (cond
                   ((not prefix) "%Y-%m-%d")
                   ((equal prefix '(4)) "%d.%m.%Y")
                   ((equal prefix '(16)) "%A, %d. %B %Y")))
          (system-time-locale "de_DE"))
      (insert (format-time-string format))))

(defun my-tag-lines (b e tag)
  "HTML: Wrap every line in the region, B -> E, with a TAG."
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

(defun my-randomize-region (beg end)
    "Randomize lines in region from BEG to END."
    (interactive "*r")
    (let ((lines (split-string
                   (delete-and-extract-region beg end) "\n")))
      (when (string-equal "" (car (last lines 1)))
        (setq lines (butlast lines 1)))
      (apply 'insert
        (mapcar 'cdr
        (sort (mapcar (lambda (x) (cons (random) (concat x "\n"))) lines)
              (lambda (a b) (< (car a) (car b))))))))

;; Generate a new org file, named after current timestamps
(defun gimme-org ()
  "Create Org file from skeleton with current time as name."
  (interactive)
  (find-file (format-time-string "~/Dropbox (Personal)/docs/gimme/%Y-%m-%d--%H-%M-%S.org"))
  (insert "#+OPTIONS: toc:nil num:nil H:4 ^:nil pri:t
#+TITLE:

")
  (insert-date nil)
  (forward-line -2)
  (end-of-line)
  (insert " "))


;; Fix foolish calendar-mode scrolling.
;; From https://sites.google.com/site/steveyegge2/my-dot-emacs-file
(add-hook 'calendar-load-hook
 '(lambda ()
    (setq mark-holidays-in-calendar t)
    (define-key calendar-mode-map ">" 'scroll-calendar-left)
    (define-key calendar-mode-map "<" 'scroll-calendar-right)))

(require 's3paste)
(setq s3paste-http-destination "http://paste.untrod.com"
      s3paste-user-address "http://www.untrod.com"
      s3paste-user-name "Chris"
      s3paste-bucket-name "paste.untrod.com")

(provide '05-misc)
;;; 05-misc.el ends here
