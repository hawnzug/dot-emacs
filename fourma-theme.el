(deftheme fourma
  "Created 2020-10-25.")

(custom-theme-set-faces
 'fourma
 '(default ((t (:family "Iosevka Curly Slab"))))
 '(cursor ((t (:background "black"))))
 '(fixed-pitch ((t (:family "Iosevka Curly Slab"))))
 '(variable-pitch ((t (:family "Merriweather"))))
 '(highlight ((t (:background "grey"))))
 '(hl-line ((t nil)))
 '(region ((t (:background "Skyblue1"))))
 '(warning ((t (:background "#FF6666"))))
 '(secondary-selection ((t (:background "light yellow"))))
 '(trailing-whitespace ((t (:background "red"))))
 '(font-lock-builtin-face ((t (:slant italic))))
 '(font-lock-comment-delimiter-face ((t (:inherit (font-lock-comment-face)))))
 '(font-lock-comment-face ((t (:background "#FFFFF0"))))
 '(font-lock-constant-face ((t nil)))
 '(font-lock-doc-face ((t (:inherit (font-lock-comment-face)))))
 '(font-lock-function-name-face ((t (:inherit (default)))))
 '(font-lock-keyword-face ((t (:slant italic))))
 '(font-lock-negation-char-face ((t nil)))
 '(font-lock-preprocessor-face ((t (:inherit (font-lock-builtin-face)))))
 '(font-lock-regexp-grouping-backslash ((t (:inherit (bold)))))
 '(font-lock-regexp-grouping-construct ((t (:inherit (bold)))))
 '(font-lock-string-face ((t (:background "#F0FFFF"))))
 '(font-lock-type-face ((t nil)))
 '(font-lock-variable-name-face ((t nil)))
 '(font-lock-warning-face ((t (:inherit (warning)))))
 '(fringe ((t (:background "white"))))

 ;; LaTeX
 '(font-latex-sedate-face ((t (:inherit (font-lock-keyword-face)))))
 '(font-latex-string-face ((t (:inherit (font-lock-string-face)))))
 '(font-latex-math-face ((t (:background "#FFF0FF"))))
 '(font-latex-script-char-face ((t nil)))

 ;; Dired
 '(dired-directory ((t (:weight bold))))

 ;; which-key
 '(which-key-separator-face ((t nil)))
 )

(provide-theme 'fourma)
