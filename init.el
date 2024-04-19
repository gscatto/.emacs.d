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

;; Install Magit, a Git porcelain inside Emacs.
;;
;; See also https://magit.vc/.
(use-package magit)

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

  ;; Get a visual signal when the bell rings.
  ;;
  ;; See also https://www.emacswiki.org/emacs/AlarmBell.
  (setq visible-bell 1))
