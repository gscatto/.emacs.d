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

;; https://github.com/slotThe/vc-use-package
(unless (package-installed-p 'vc-use-package)
  (package-vc-install "https://github.com/slotThe/vc-use-package"))

(use-package vertico
  :vc (vertico :url "https://github.com/minad/vertico"
	       :lisp-dir "extensions/")
  :config
  (vertico-mode)
  (vertico-reverse-mode))

(use-package orderless
  :vc (orderless :url "https://github.com/oantolin/orderless")
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
