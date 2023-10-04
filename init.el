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

(use-package diff-hl
  :hook (((emacs-lisp-mode . diff-hl-mode))
	 (magit-pre-refresh . diff-hl-magit-pre-refresh)
	 (magit-post-refresh . diff-hl-magit-post-refresh))
  :config
  (diff-hl-flydiff-mode t)
  (diff-hl-show-hunk-mouse-mode t)
  :custom
  (diff-hl-show-staged-changes nil))

;; Make the cursor a thin bar.
(setq-default cursor-type 'bar)

(save-place-mode t)

(use-package corfu
  :init
  (global-corfu-mode t)
  :custom
  (completion-cycle-threshold 3))

(use-package undo-tree
  :config
  (global-undo-tree-mode t)
  (setq undo-tree-history-directory-alist `((".*" . ,(expand-file-name "tmp/undo-tree/" user-emacs-directory)))))

(use-package volatile-highlights
  :init
  (volatile-highlights-mode t)
  ;; undo-tree
  (vhl/define-extension 'undo-tree 'undo-tree-yank 'undo-tree-move)
  (vhl/install-extension 'undo-tree))

(define-minor-mode recompile-after-save-mode
  "A minor mode that recompiles after save."
  :init-value nil
  (if recompile-after-save-mode
      (add-hook 'after-save-hook 'recompile)
    (remove-hook 'after-save-hook 'recompile)))

(define-minor-mode ask-to-quit-compilation-window-mode
  "A minor mode that asks to quit the compilation window."
  :init-value nil
  (let ((hook (lambda (buffer message)
		(when (y-or-n-p "Quit *compilation* window? ")
		  (quit-window nil (get-buffer-window buffer nil))))))
    (if ask-to-quit-compilation-window-mode
	(add-hook 'compilation-finish-functions hook)
      (remove-hook 'compilation-finish-functions hook))))

(define-minor-mode auto-quit-compilation-window-mode
  "A minor mode that automatically quits the compilation window when finished without warnings."
  :init-value nil
  (let ((hook (lambda (buffer message)
		(when (and (buffer-live-p buffer)
			   (string-match "compilation" (buffer-name buffer))
			   (string-match "finished" message)
			   (not (with-current-buffer buffer
				  (goto-char (point-min))
				  (search-forward "warning" nil t))))
		  (run-with-timer 1 nil (lambda (buffer)
					  (quit-window nil (get-buffer-window buffer nil)))
				  buffer)))))
    (if auto-quit-compilation-window-mode
	(add-hook 'compilation-finish-functions hook)
      (remove-hook 'compilation-finish-functions hook))))

;; Support ANSI coloring in compilation mode.
(use-package ansi-color
  :hook (compilation-filter . ansi-color-compilation-filter))

;; Hide the tool bar.
(when (fboundp 'tool-bar-mode)
  (tool-bar-mode -1))

;; The mode line face inherits variable pitch.
(set-face-attribute 'mode-line nil :inherit 'variable-pitch)

(use-package org-roam
  :bind (("C-c n f" . org-roam-node-find)
	 ("C-c n i" . org-roam-node-insert)
	 ("C-c n c" . org-roam-capture)
	 ("C-c n r" . org-roam-node-random))
  :custom
  (org-roam-database-connector 'sqlite-builtin))

(use-package embark
  :bind (("<mouse-3>" . embark-act)
	 ("C-." . embark-act)
	 ("C-;" . embark-dwim)
	 ("C-c e a" . embark-act)
	 ("C-c e d" . embark-dwim)))

(use-package org
  :straight (:type built-in)
  :custom
  (org-attach-store-link-p 'attached))
