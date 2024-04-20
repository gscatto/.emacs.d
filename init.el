;; Install straight.el, a next-generation, purely functional package
;; manager for the Emacs hacker.
;;
;; See also https://github.com/radian-software/straight.el
;;
;; Configure straight.el before installation.
;;
;; Make each use-package form also invoke straight.el to install the
;; package, unless otherwise specified.
(setq straight-use-package-by-default t)

;; Install it.
(defvar bootstrap-version)
(let ((bootstrap-file
       (expand-file-name
        "straight/repos/straight.el/bootstrap.el"
        (or (bound-and-true-p straight-base-dir)
            user-emacs-directory)))
      (bootstrap-version 7))
  (unless (file-exists-p bootstrap-file)
    (with-current-buffer
        (url-retrieve-synchronously
         "https://raw.githubusercontent.com/radian-software/straight.el/develop/install.el"
         'silent 'inhibit-cookies)
      (goto-char (point-max))
      (eval-print-last-sexp)))
  (load bootstrap-file nil 'nomessage))

;; Install use-package with straight.el.
(straight-use-package 'use-package)

;; Install packages automatically if not already present on our
;; system.  See also https://github.com/jwiegley/use-package.
(require 'use-package-ensure)
(setq use-package-always-ensure t)

(use-package org
  :config
  ;; Make org-babel to load support for languages when I actually try
  ;; to run a code block with that language.
  ;;
  ;; Source: https://emacs.stackexchange.com/q/20577
  (defadvice org-babel-execute-src-block (around load-language nil activate)
    (let ((language (org-element-property :language (org-element-at-point))))
      (unless (cdr (assoc (intern language) org-babel-load-languages))
	(add-to-list 'org-babel-load-languages (cons (intern language) t))
	(org-babel-do-load-languages 'org-babel-load-languages org-babel-load-languages))
      ad-do-it))

  ;; Org does not prompt the user for confirmation before executing
  ;; each code block.
  ;;
  ;; See https://orgmode.org/manual/Code-Evaluation-Security.html for
  ;; more information.
  (setq org-confirm-babel-evaluate nil))

;; Install Magit, a Git porcelain inside Emacs.
;;
;; See also https://magit.vc/.
(use-package magit
  :config
  ;; Refresh the status buffer after a buffer is saved.
  (add-hook 'after-save-hook 'magit-after-save-refresh-status t))

;; Install vertico.el - VERTical Interactive COmpletion.
;;
;; See also https://github.com/minad/vertico.
(use-package vertico
  :init
  (vertico-mode))

;; Persist history over Emacs restarts.
(use-package savehist
  :init
  (savehist-mode))

;; Configure Emacs
(use-package emacs
  :init
  ;; Add prompt indicator to `completing-read-multiple'. We display
  ;; [CRM<separator>], e.g., [CRM,] if the separator is a comma.
  (defun crm-indicator (args)
    (cons (format "[CRM%s] %s"
                  (replace-regexp-in-string
                   "\\`\\[.*?]\\*\\|\\[.*?]\\*\\'" ""
                   crm-separator)
                  (car args))
          (cdr args)))
  (advice-add #'completing-read-multiple :filter-args #'crm-indicator)

  ;; Do not allow the cursor in the minibuffer prompt
  (setq minibuffer-prompt-properties
        '(read-only t cursor-intangible t face minibuffer-prompt))
  (add-hook 'minibuffer-setup-hook #'cursor-intangible-mode)

  ;; Support opening new minibuffers from inside existing minibuffers.
  (setq enable-recursive-minibuffers t)

  ;; Emacs 28 and newer: Hide commands in M-x which do not work in the
  ;; current mode.  Vertico commands are hidden in normal
  ;; buffers. This setting is useful beyond Vertico.
  (setq read-extended-command-predicate #'command-completion-default-include-p)

  ;; Flash the Emacs mode line instead of ringing the bell.
  ;;
  ;; https://github.com/purcell/mode-line-bell
  (use-package mode-line-bell
    :config
    (mode-line-bell-mode 1))

  ;; Save customizations other than this initialization file.
  (setq custom-file (concat user-emacs-directory "custom.el"))
  (load custom-file t)

  ;; Avoid creating backup files.
  (setq make-backup-files nil)

  ;; Disable the toolbar.
  (tool-bar-mode -1)

  ;; Avoid creating lockfiles.
  (setq create-lockfiles nil))

;; Install Orderless, an Emacs completion style that matches multiple
;; regexps in any order. See https://github.com/oantolin/orderless for
;; more information.
(use-package orderless
  :ensure t
  :custom
  (completion-styles '(orderless basic))
  (completion-category-overrides '((file (styles basic partial-completion)))))

;; Install Marginalia, an Emacs package that adds marginalia to the
;; minibuffer completions. See https://github.com/minad/marginalia for
;; more information.
(use-package marginalia
  :init
  (marginalia-mode))

;; Install consult.el - Consulting completing-read. See also
;; https://github.com/minad/consult for more information.
(use-package consult
  ;; Replace bindings. Lazily loaded due by `use-package'.
  :bind (;; C-c bindings in `mode-specific-map'
         ("C-c M-x" . consult-mode-command)
         ("C-c h" . consult-history)
         ("C-c k" . consult-kmacro)
         ("C-c m" . consult-man)
         ("C-c i" . consult-info)
         ([remap Info-search] . consult-info)
         ;; C-x bindings in `ctl-x-map'
         ("C-x M-:" . consult-complex-command)     ;; orig. repeat-complex-command
         ("C-x b" . consult-buffer)                ;; orig. switch-to-buffer
         ("C-x 4 b" . consult-buffer-other-window) ;; orig. switch-to-buffer-other-window
         ("C-x 5 b" . consult-buffer-other-frame)  ;; orig. switch-to-buffer-other-frame
         ("C-x t b" . consult-buffer-other-tab)    ;; orig. switch-to-buffer-other-tab
         ("C-x r b" . consult-bookmark)            ;; orig. bookmark-jump
         ("C-x p b" . consult-project-buffer)      ;; orig. project-switch-to-buffer
         ;; Custom M-# bindings for fast register access
         ("M-#" . consult-register-load)
         ("M-'" . consult-register-store)          ;; orig. abbrev-prefix-mark (unrelated)
         ("C-M-#" . consult-register)
         ;; Other custom bindings
         ("M-y" . consult-yank-pop)                ;; orig. yank-pop
         ;; M-g bindings in `goto-map'
         ("M-g e" . consult-compile-error)
         ("M-g f" . consult-flymake)               ;; Alternative: consult-flycheck
         ("M-g g" . consult-goto-line)             ;; orig. goto-line
         ("M-g M-g" . consult-goto-line)           ;; orig. goto-line
         ("M-g o" . consult-outline)               ;; Alternative: consult-org-heading
         ("M-g m" . consult-mark)
         ("M-g k" . consult-global-mark)
         ("M-g i" . consult-imenu)
         ("M-g I" . consult-imenu-multi)
         ;; M-s bindings in `search-map'
         ("M-s d" . consult-find)                  ;; Alternative: consult-fd
         ("M-s c" . consult-locate)
         ("M-s g" . consult-grep)
         ("M-s G" . consult-git-grep)
         ("M-s r" . consult-ripgrep)
         ("M-s l" . consult-line)
         ("M-s L" . consult-line-multi)
         ("M-s k" . consult-keep-lines)
         ("M-s u" . consult-focus-lines)
         ;; Isearch integration
         ("M-s e" . consult-isearch-history)
         :map isearch-mode-map
         ("M-e" . consult-isearch-history)         ;; orig. isearch-edit-string
         ("M-s e" . consult-isearch-history)       ;; orig. isearch-edit-string
         ("M-s l" . consult-line)                  ;; needed by consult-line to detect isearch
         ("M-s L" . consult-line-multi)            ;; needed by consult-line to detect isearch
         ;; Minibuffer history
         :map minibuffer-local-map
         ("M-s" . consult-history)                 ;; orig. next-matching-history-element
         ("M-r" . consult-history))                ;; orig. previous-matching-history-element

  ;; Enable automatic preview at point in the *Completions* buffer. This is
  ;; relevant when you use the default completion UI.
  :hook (completion-list-mode . consult-preview-at-point-mode)

  ;; The :init configuration is always executed (Not lazy)
  :init

  ;; Optionally configure the register formatting. This improves the register
  ;; preview for `consult-register', `consult-register-load',
  ;; `consult-register-store' and the Emacs built-ins.
  (setq register-preview-delay 0.5
        register-preview-function #'consult-register-format)

  ;; Optionally tweak the register preview window.
  ;; This adds thin lines, sorting and hides the mode line of the window.
  (advice-add #'register-preview :override #'consult-register-window)

  ;; Use Consult to select xref locations with preview
  (setq xref-show-xrefs-function #'consult-xref
        xref-show-definitions-function #'consult-xref)

  ;; Configure other variables and modes in the :config section,
  ;; after lazily loading the package.
  :config

  ;; Optionally configure preview. The default value
  ;; is 'any, such that any key triggers the preview.
  ;; (setq consult-preview-key 'any)
  ;; (setq consult-preview-key "M-.")
  ;; (setq consult-preview-key '("S-<down>" "S-<up>"))
  ;; For some commands and buffer sources it is useful to configure the
  ;; :preview-key on a per-command basis using the `consult-customize' macro.
  (consult-customize
   consult-theme :preview-key '(:debounce 0.2 any)
   consult-ripgrep consult-git-grep consult-grep
   consult-bookmark consult-recent-file consult-xref
   consult--source-bookmark consult--source-file-register
   consult--source-recent-file consult--source-project-recent-file
   ;; :preview-key "M-."
   :preview-key '(:debounce 0.4 any))

  ;; Optionally configure the narrowing key.
  ;; Both < and C-+ work reasonably well.
  (setq consult-narrow-key "<") ;; "C-+"

  ;; Optionally make narrowing help available in the minibuffer.
  ;; You may want to use `embark-prefix-help-command' or which-key instead.
  ;; (define-key consult-narrow-map (vconcat consult-narrow-key "?") #'consult-narrow-help)
  )

;; Install markdown-mode, a mode for Markdown files. See
;; https://jblevins.org/projects/markdown-mode/ for an in-depth
;; explanation of its capabilities.
(use-package markdown-mode
  :init
  (setq markdown-display-remote-images t
	markdown-max-image-size '(320 . 240)))

;; Diff-Hl-Mode highlights uncommitted changes on the left side of the
;; window. See https://github.com/dgutov/diff-hl for more information.
(use-package diff-hl
  :hook
  ((org-mode prog-mode) . diff-hl-mode)
  (magit-pre-refresh-hook . diff-hl-magit-pre-refresh)
  (magit-post-refresh . diff-hl-magit-post-refresh)
  :config
  (global-diff-hl-mode 1)
  (diff-hl-flydiff-mode 1))

;; Make Emacs use the $PATH set up by the user's shell. See
;; https://github.com/purcell/exec-path-from-shell for more
;; information.
(use-package exec-path-from-shell
  :init
  (exec-path-from-shell-initialize))

;; Remember recently edited files.
(recentf-mode 1)

;; Prevent using UI dialogs for prompts.
(setq use-dialog-box nil)

;; Always use "y" or "n" for yes-no responses.
(defalias 'yes-or-no-p 'y-or-n-p)

;; Automatically revert buffers for changed files.
(global-auto-revert-mode 1)

;; Automatically revert changed file buffers and custom bufffers
(setq global-auto-revert-non-file-buffers 1)

;; Auto-Revert-Mode performs checks every half a second.
(setq auto-revert-interval 0.5)

;; Enable pixel-scroll-precision-mode if present
(when (fboundp 'pixel-scroll-precision-mode)
  (pixel-scroll-precision-mode 1))

;; Typed text replaces the active selection.
(delete-selection-mode 1)

;; Make the cursor a thin bar
(setq-default cursor-type 'bar)

;; Automatically save place in each file.
(save-place-mode 1)

;; Install undo-tree, an Emacs package that treats undo history as a
;; tree.
;;
;; https://github.com/emacsmirror/undo-tree/blob/master/undo-tree.el
(use-package undo-tree
  :config
  (global-undo-tree-mode 1)
  (setq undo-tree-history-directory-alist '(("." . "~/.emacs.d/undo"))))

;; Make TAB intelligent: it tries to indent the current line first and
;; if the line was already indented, then try to complete the thing at
;; point.
(setq tab-always-indent 'complete)

;; Install corfu.el - COmpletion in Region FUnction.
;;
;; Corfu enhances in-buffer completion with a small completion
;; popup. The current candidates are shown in a popup below or above
;; the point. The candidates can be selected by moving up and down.
;;
;; https://github.com/minad/corfu
(use-package corfu
  :init
  (global-corfu-mode 1)
  :custom
  (completion-cycle-threshold 3))

;; Install which-key, an Emacs package that displays available
;; keybindings in popup.
;;
;; https://github.com/justbur/emacs-which-key
(use-package which-key
  :config
  (which-key-mode 1))

;; Install Ace-Jump-Mode, an Emacs package which helps you to move the
;; cursor within Emacs by using only 3 times key press.
;;
;; https://github.com/winterTTr/ace-jump-mode
(use-package ace-jump-mode
  :bind (("C-c SPC" . ace-jump-mode)
	 ("C-c c" . ace-jump-mode-pop-mark)))

;; https://github.com/magit/magit-annex
(use-package magit-annex)
