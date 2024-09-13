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
