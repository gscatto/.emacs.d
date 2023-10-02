;; Avoid polluting this file with customization information.
(setq custom-file (expand-file-name "custom.el" user-emacs-directory))
(when (file-exists-p custom-file)
  (load custom-file))

;; Avoid polluting working directories with file backups.
(setq backup-directory-alist `(("." . ,(expand-file-name "tmp/backups/" user-emacs-directory))))
