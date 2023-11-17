(deftheme void
  "Created 2023-11-17.")

(custom-theme-set-faces
 'void
 '(org-block ((t (:inherit fixed-pitch :extend t))))
 '(fringe ((t nil)))
 '(mode-line ((t (:inherit variable-pitch :background "white" :foreground "black" :box nil))))
 '(mode-line-inactive ((t (:inherit mode-line :foreground "grey" :box nil :weight light))))
 '(vertical-border ((t (:foreground "white")))))

(provide-theme 'void)
