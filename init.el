;; Remap the Command key on macOS to Ctrl.
;;
;; Source: https://www.reddit.com/r/emacs/comments/isl1s5/remapping_the_command_key_on_macos_to_ctrl/
(custom-set-variables
 '(ns-alternate-modifier 'meta)
 '(ns-right-alternate-modifier 'meta) 
 '(ns-command-modifier 'control)
 '(ns-right-command-modifier 'control))

;; Save customization in a dedicated file.
(setq custom-file (concat user-emacs-directory "custom.el"))

;; Load customization if the file exists.
(when (file-exists-p custom-file)
  (load custom-file nil t))

;; Install Elpaca, a package manager for Emacs.
;; https://github.com/progfolio/elpaca
(defvar elpaca-installer-version 0.7)
(defvar elpaca-directory (expand-file-name "elpaca/" user-emacs-directory))
(defvar elpaca-builds-directory (expand-file-name "builds/" elpaca-directory))
(defvar elpaca-repos-directory (expand-file-name "repos/" elpaca-directory))
(defvar elpaca-order '(elpaca :repo "https://github.com/progfolio/elpaca.git"
                              :ref nil :depth 1
                              :files (:defaults "elpaca-test.el" (:exclude "extensions"))
                              :build (:not elpaca--activate-package)))
(let* ((repo  (expand-file-name "elpaca/" elpaca-repos-directory))
       (build (expand-file-name "elpaca/" elpaca-builds-directory))
       (order (cdr elpaca-order))
       (default-directory repo))
  (add-to-list 'load-path (if (file-exists-p build) build repo))
  (unless (file-exists-p repo)
    (make-directory repo t)
    (when (< emacs-major-version 28) (require 'subr-x))
    (condition-case-unless-debug err
        (if-let ((buffer (pop-to-buffer-same-window "*elpaca-bootstrap*"))
                 ((zerop (apply #'call-process `("git" nil ,buffer t "clone"
                                                 ,@(when-let ((depth (plist-get order :depth)))
                                                     (list (format "--depth=%d" depth) "--no-single-branch"))
                                                 ,(plist-get order :repo) ,repo))))
                 ((zerop (call-process "git" nil buffer t "checkout"
                                       (or (plist-get order :ref) "--"))))
                 (emacs (concat invocation-directory invocation-name))
                 ((zerop (call-process emacs nil buffer nil "-Q" "-L" "." "--batch"
                                       "--eval" "(byte-recompile-directory \".\" 0 'force)")))
                 ((require 'elpaca))
                 ((elpaca-generate-autoloads "elpaca" repo)))
            (progn (message "%s" (buffer-string)) (kill-buffer buffer))
          (error "%s" (with-current-buffer buffer (buffer-string))))
      ((error) (warn "%s" err) (delete-directory repo 'recursive))))
  (unless (require 'elpaca-autoloads nil t)
    (require 'elpaca)
    (elpaca-generate-autoloads "elpaca" repo)
    (load "./elpaca-autoloads")))
(add-hook 'after-init-hook #'elpaca-process-queues)
(elpaca `(,@elpaca-order))

;; Install use-package support
(elpaca elpaca-use-package
  ;; Enable use-package :ensure support for Elpaca.
  (elpaca-use-package-mode))

;; In GNU Emacs 29.0.50, Magit is asking for a seq version newer than
;; what Emacs ships. This unloads seq in order to reinstall the
;; upgraded version without causing warnings.
;;
;; https://www.reddit.com/r/emacs/comments/1937vaz/comment/kh8vxhb
(defun +elpaca-unload-seq (e)
  (and (featurep 'seq) (unload-feature 'seq t))
  (elpaca--continue-build e))

(defun +elpaca-seq-build-steps ()
  (append (butlast (if (file-exists-p (expand-file-name "seq" elpaca-builds-directory))
                       elpaca--pre-built-steps elpaca-build-steps))
          (list '+elpaca-unload-seq 'elpaca--activate-package)))

(use-package seq :ensure `(seq :build ,(+elpaca-seq-build-steps)))

;; Install Magit, a Git porcelain inside Emacs.
;;
;; See also https://magit.vc/.
(use-package magit
  :ensure t
  :after seq
  :config
  ;; Refresh the status buffer after a buffer is saved.
  (add-hook 'after-save-hook 'magit-after-save-refresh-status t))

;; Install vertico.el - VERTical Interactive COmpletion.
;;
;; See also https://github.com/minad/vertico.
(use-package vertico
  :ensure t
  :init
  (vertico-mode))

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
  :ensure t
  :init
  (marginalia-mode))

;; Install consult.el - Consulting completing-read. See also
;; https://github.com/minad/consult for more information.
(use-package consult
  :ensure t
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

;; Persist history over Emacs restarts.
(use-package savehist
  :init
  (savehist-mode))

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
  :ensure t
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
  :ensure t
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
  :ensure t
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
  :ensure t
  :init
  (global-corfu-mode 1)
  :custom
  (completion-cycle-threshold 3))

;; Install which-key, an Emacs package that displays available
;; keybindings in popup.
;;
;; https://github.com/justbur/emacs-which-key
(use-package which-key
  :ensure t
  :config
  (which-key-mode 1))

;; Install Denote, simple notes for Emacs with an efficient
;; file-naming scheme.
;;
;; https://protesilaos.com/emacs/denote
(use-package denote
  :ensure t
  :bind (("C-c n n" . denote-open-or-create)
	 ("C-c n c" . denote-region)
	 ("C-c n N" . denote-type)
	 ("C-c n d" . denote-date)
	 ("C-c n z" . denote-signature)
	 ("C-c n s" . denote-subdirectory)
	 ("C-c n t" . denote-template)
	 ("C-c n i" . denote-link-or-create)
	 ("C-c n I" . denote-add-links)
	 ("C-c n b" . denote-backlinks)
	 ("C-c n f f" . denote-find-link)
	 ("C-c n f b" . denote-find-backlink)
	 ("C-c n r" . denote-rename-file)
	 ("C-c n R" . denote-rename-file-using-front-matter)
	 (:map dired-mode-map
	       ("C-c C-d C-i" . denote-link-dired-marked-notes)
	       ("C-c C-d C-r" . denote-dired-rename-files)
	       ("C-c C-d C-k" . denote-dired-rename-marked-files-with-keywords)
	       ("C-c C-d C-R" . denote-dired-rename-marked-files-using-front-matter))))

;; "Compile on save" in Emacs.
;;
;; https://rtime.ciirc.cvut.cz/~sojka/blog/compile-on-save/
(defun compile-on-save-start ()
  (let ((buffer (compilation-find-buffer)))
    (unless (get-buffer-process buffer)
      (recompile))))

(define-minor-mode compile-on-save-mode
  "Minor mode to automatically call `recompile' whenever the
current buffer is saved. When there is ongoing compilation,
nothing happens."
  :lighter " CoS"
  (if compile-on-save-mode
      (progn  (make-local-variable 'after-save-hook)
              (add-hook 'after-save-hook 'compile-on-save-start nil t))
    (kill-local-variable 'after-save-hook)))

;; Emacs major mode for the Meson build system.
;;
;; https://github.com/wentasah/meson-mode
(use-package meson-mode
  :ensure t)

;; A PlantUML major mode for Emacs.
;;
;; https://github.com/skuro/plantuml-mode
(use-package plantuml-mode
  :ensure t)

;; https://stackoverflow.com/a/71785402/10750781
(use-package ansi-color
    :hook (compilation-filter . ansi-color-compilation-filter))

;; Place backup and auto-save files in the system's temporary
;; directory.
(setq backup-directory-alist
      `((".*" . ,temporary-file-directory)))
(setq auto-save-file-name-transforms
      `((".*" ,temporary-file-directory t)))

;; Activate variable-pitch-mode in Org files
(add-hook 'org-mode-hook 'variable-pitch-mode)
