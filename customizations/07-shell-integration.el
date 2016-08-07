;;; shell --- mostly just imports env vars using exec-path-from-shell

;;; Commentary:
;; https://github.com/purcell/exec-path-from-shell

;;; Code:

(when (memq window-system '(mac ns))
  (exec-path-from-shell-initialize)
  (exec-path-from-shell-copy-envs
   '("PATH")))

;; shell scripts
(setq-default sh-basic-offset 2)
(setq-default sh-indentation 2)

(provide '07-shell-integration)
;;; 07-shell-integration ends here
