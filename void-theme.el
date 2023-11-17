(deftheme void
  "Created 2023-11-17.")

(custom-theme-set-variables
 'void
 '(default-frame-alist '((vertical-scroll-bars) (internal-border-width . 8)))
 '(minibuffer-frame-alist '((width . 80) (height . 2))))

(custom-theme-set-faces
 'void
 '(org-block ((t (:inherit fixed-pitch :extend t))))
 '(fringe ((t nil)))
 '(mode-line ((t (:box (:line-width 8 :color "white") :foreground "black" :background "white" :inherit variable-pitch))))
 '(mode-line-inactive ((t (:inherit mode-line :foreground "grey" :box nil :weight light))))
 '(vertical-border ((t (:foreground "white"))))
 '(diff-hl-change ((t (:foreground "#ddddff" :background "#ddddff"))))
 '(diff-hl-delete ((t
		    (:foreground "#ffeeee" :inherit
				 (diff-removed)))))
 '(diff-hl-insert ((default
		    (:inherit
		     (diff-added)))
		   (((class color))
		    (:foreground "#eeffee"))))
 '(minibuffer-prompt ((((background dark)) (:foreground "cyan")) (((type pc)) (:foreground "magenta")) (t (:foreground "medium blue")))))


(defun void/decorate-echo-area ()
  (dolist (buffer-name '(" *Echo Area 0*" " *Echo Area 1*"))
    (let ((buffer (get-buffer buffer-name)))
      (when buffer
	(with-current-buffer buffer
	  (void/decorate-minibuffer))))))
(defun void/decorate-minibuffer ()
  (setq-local face-remapping-alist '((default variable-pitch))))
;; (add-hook 'echo-area-clear-hook 'void/decorate-echo-area)
;; (add-hook 'minibuffer-setup-hook 'void/decorate-minibuffer)

(provide-theme 'void)
