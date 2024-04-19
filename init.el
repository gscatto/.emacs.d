;; Get a visual signal when the bell rings.
;;
;; See also https://www.emacswiki.org/emacs/AlarmBell.
(setq visible-bell 1)

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
;;
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
;;
;; Install use-package with straight.el.
(straight-use-package 'use-package)
;;
;; Install packages automatically if not already present on our
;; system.  See also https://github.com/jwiegley/use-package.
(require 'use-package-ensure)
(setq use-package-always-ensure t)

;; Install Magit, a Git porcelain inside Emacs.
;;
;; See also https://magit.vc/.
(use-package magit)
