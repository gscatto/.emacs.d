;; Avoid polluting this file with customization information.
(setq custom-file (expand-file-name "custom.el" user-emacs-directory))
(when (file-exists-p custom-file)
  (load custom-file))

;; Avoid polluting working directories with file backups.
(setq backup-directory-alist `(("." . ,(expand-file-name "tmp/backups/" user-emacs-directory))))

;; Remember recently edited files.
(recentf-mode +1)

;; Remember minibuffer prompt history.
(savehist-mode +1)

;; Prevent using UI dialogs for prompts.
(setq use-dialog-box nil)

;; Always use `y` or `n` for yes-no responses.
(defalias 'yes-or-no-p 'y-or-n-p)
