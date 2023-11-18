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
 '(mode-line-inactive ((t (:weight light :foreground "grey" :inherit mode-line))))
 '(vertical-border ((t (:foreground "white"))))
 '(diff-hl-change ((t (:foreground "#ddddff" :background "#ddddff"))))
 '(diff-hl-delete ((t (:foreground "#ffeeee" :inherit (diff-removed)))))
 '(diff-hl-insert ((default (:inherit (diff-added))) (((class color)) (:foreground "#eeffee"))))
 '(minibuffer-prompt ((((background dark)) (:foreground "cyan")) (((type pc)) (:foreground "magenta")) (t (:foreground "medium blue")))))

(provide-theme 'void)
