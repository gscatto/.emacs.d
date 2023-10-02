;; Avoid polluting this file with customization information.
(setq custom-file (expand-file-name "custom.el" user-emacs-directory))
(when (file-exists-p custom-file)
  (load custom-file))

;; Avoid polluting working directories with file backups.
(setq backup-directory-alist `(("." . ,(expand-file-name "tmp/backups/" user-emacs-directory))))

;; Remember recently edited files.
(recentf-mode t)

;; Remember minibuffer prompt history.
(savehist-mode t)

;; Prevent using UI dialogs for prompts.
(setq use-dialog-box nil)

;; Always use `y` or `n` for yes-no responses.
(defalias 'yes-or-no-p 'y-or-n-p)

;; Automatically revert buffers for changed files.
(global-auto-revert-mode t)

;; Bootstrap straight.el
(defvar bootstrap-version)
(let ((bootstrap-file
       (expand-file-name "straight/repos/straight.el/bootstrap.el" user-emacs-directory))
      (bootstrap-version 6))
  (unless (file-exists-p bootstrap-file)
    (with-current-buffer
        (url-retrieve-synchronously
         "https://raw.githubusercontent.com/radian-software/straight.el/develop/install.el"
         'silent 'inhibit-cookies)
      (goto-char (point-max))
      (eval-print-last-sexp)))
  (load bootstrap-file nil 'nomessage))

(unless (package-installed-p 'use-package)
  (straight-use-package 'use-package))

;; By default install packages in use-package forms.
(setq straight-use-package-by-default t)

(use-package vertico
  :config
  (vertico-mode)
  (vertico-reverse-mode))

(use-package orderless
  :custom
  (completion-styles '(orderless basic))
  (completion-category-overrides '((file (styles basic partial-completion)))))

;; TAB first tries to indent the current line, and if the line was already
;; indented, then try to complete the thing at point.
(setq tab-always-indent 'complete)

;; Maximize the Emacs frame on startup.
(add-hook 'window-setup-hook 'toggle-frame-maximized t)

(when (fboundp 'pixel-scroll-precision-mode)
  (pixel-scroll-precision-mode t))

;; Display the current line number in the mode line.
(line-number-mode t)

;; Display the current column number in the mode line.
(column-number-mode t)

;; Display the buffer size in the mode line.
(size-indication-mode t)

;; Typed text replaces the active selection.
(delete-selection-mode t)

(use-package magit
  :config
  (add-hook 'after-save-hook 'magit-after-save-refresh-status t))
