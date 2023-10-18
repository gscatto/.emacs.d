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

;; Automatically revert changed file and custom buffers.
(setq global-auto-revert-non-file-buffers t)

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
  :hook (((emacs-lisp-mode ruby-mode) . diff-hl-mode)
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
	 ("C-;" . embark-dwim)))

(use-package org
  :straight (:type built-in)
  :bind (("C-c l" . org-store-link))
  :hook ((org-mode . variable-pitch-mode)
	 (org-mode . visual-line-mode))
  :custom
  (org-attach-store-link-p 'attached)
  (org-id-link-to-org-use-id t))

(use-package eglot
  :straight (:type built-in)
  :bind (:map eglot-mode-map ("C-c e" . eglot-code-action-extract)
	      ("C-c i" . eglot-code-action-inline)
	      ("C-c q" . eglot-code-action-quickfix)
	      ("C-c r" . eglot-rename)
	      ("C-c a" . eglot-code-actions)))

(if theme (load-theme theme t))

(use-package visual-fill-column
  :custom (visual-fill-column-center-text t))

;; http://malsyned.net/smart-dash.html
;;
;; Smart-Dash mode is an Emacs minor mode which redefines the dash key
;; to insert an underscore within C-style identifiers and a dash
;; otherwise.
(use-package smart-dash
  :hook ((ruby-mode python-mode rustic-mode) . smart-dash-mode))

;; https://github.com/purcell/exec-path-from-shell
;;
;; A GNU Emacs library to ensure environment variables inside Emacs
;; look the same as in the user's shell.
(use-package exec-path-from-shell
  :config
  (exec-path-from-shell-initialize))

;; Do Auto-Revert Mode file checks every half a second.
(setq auto-revert-interval 0.5)

(use-package rustic
  :config
  ;; Use Eglot as LSP client
  (setq rustic-lsp-client 'eglot))

;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;
;; Display-Time-Since-First-Change mode
(define-minor-mode display-time-since-first-change-mode
  "A minor mode that displays the time elapsed since the first change in the buffer."
  :init-value nil
  (when (timerp display-time-since-first-change-mode/timer)
    (cancel-timer display-time-since-first-change-mode/timer)
    (setq display-time-since-first-change-mode/timer nil))
  (if display-time-since-first-change-mode (display-time-since-first-change-mode/install) (display-time-since-first-change-mode/uninstall)))

(defun display-time-since-first-change-mode/install ()
  (add-hook 'first-change-hook 'display-time-since-first-change-mode/after-first-change nil t)
  (add-hook 'after-save-hook 'display-time-since-first-change-mode/after-save nil t)
  (unless (memq 'display-time-since-first-change-mode/mode-line-text mode-line-format)
    (setq mode-line-format (nconc mode-line-format (list 'display-time-since-first-change-mode/mode-line-text))))
  (setq display-time-since-first-change-mode/timer (run-with-timer 0 1 #'display-time-since-first-change-mode/update-modeline))
  (message "Display-Time-Since-First-Change mode enabled"))

(defun display-time-since-first-change-mode/uninstall ()
  (remove-hook 'first-change-hook 'display-time-since-first-change-mode/after-first-change)
  (setq mode-line-format (delq 'display-time-since-first-change-mode/mode-line-text mode-line-format))
  (message "Display-Time-Since-First-Change mode disabled"))

(defun display-time-since-first-change-mode/after-first-change ()
  (setq display-time-since-first-change-mode/first-change-time (current-time)))

(defun display-time-since-first-change-mode/after-save ()
  (setq display-time-since-first-change-mode/first-change-time nil))

(defun display-time-since-first-change-mode/update-modeline ()
  (unless (buffer-modified-p)
    (setq display-time-since-first-change-mode/first-change-time nil))
  (when display-time-since-first-change-mode
    (message (buffer-name))
    (setq display-time-since-first-change-mode/mode-line-text (if display-time-since-first-change-mode/first-change-time (format-time-string "%M:%S" (time-subtract (current-time) display-time-since-first-change-mode/first-change-time) 0) "00:00"))
    (force-mode-line-update t)))

(make-variable-buffer-local
 (defvar display-time-since-first-change-mode/first-change-time nil
   "The time of the first change of the current buffer."))

(make-variable-buffer-local
 (defvar display-time-since-first-change-mode/mode-line-text ""
   "What it is actually displayed in the modeline."))

(make-variable-buffer-local
 (defvar display-time-since-first-change-mode/timer nil
   "Timer updating the modeline."))
;; Display-Time-Since-First-Change mode
;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;

(use-package flymake
  :straight (:type built-in)
  :bind (("M-n" . flymake-goto-next-error)
	 ("M-p" . flymake-goto-prev-error)))

(use-package graphviz-dot-mode)

(use-package which-key
  :config
  (which-key-mode t))

;; Stop asking whether to revert a file or not.
(setq revert-without-query '(".*"))

;; Make Emacs write all the lock files to /var/tmp/
;; https://www.gnu.org/software/emacs/manual/html_node/elisp/File-Locks.html
(setq lock-file-name-transforms
      '(("\\`/.*/\\([^/]+\\)\\'" "/var/tmp/\\1" t)))

;; Ask Emacs not to make backup files
(setq make-backup-files nil)

(use-package tuareg)
