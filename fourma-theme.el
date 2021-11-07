(deftheme fourma
  "Created 2020-10-25.")

(custom-theme-set-faces
 'fourma
 '(default ((t (:family "Iosevka Curly Slab"))))
 '(fixed-pitch ((t (:family "Iosevka Curly Slab"))))
 '(variable-pitch ((t (:family "Iosevka Curly Slab"))))
 '(cursor ((t (:background "black"))))
 '(highlight ((t (:background "#FFAAAA"))))
 '(hl-line ((t nil)))
 '(region ((t (:background "#84AEE3"))))
 '(warning ((t (:background "#FF6666"))))
 '(secondary-selection ((t (:background "light yellow"))))
 '(trailing-whitespace ((t (:background "red"))))
 '(font-lock-builtin-face ((t (:slant italic))))
 '(font-lock-comment-delimiter-face ((t (:inherit (font-lock-comment-face)))))
 '(font-lock-comment-face ((t (:background "#AAF0D1"))))
 '(font-lock-constant-face ((t nil)))
 '(font-lock-doc-face ((t (:inherit (font-lock-comment-face)))))
 '(font-lock-function-name-face ((t nil)))
 '(font-lock-keyword-face ((t nil)))
 '(font-lock-negation-char-face ((t nil)))
 '(font-lock-preprocessor-face ((t (:inherit (font-lock-builtin-face)))))
 '(font-lock-regexp-grouping-backslash ((t (:inherit (bold)))))
 '(font-lock-regexp-grouping-construct ((t (:inherit (bold)))))
 '(font-lock-string-face ((t nil)))
 '(font-lock-type-face ((t (:weight bold))))
 '(font-lock-variable-name-face ((t nil)))
 '(font-lock-warning-face ((t (:inherit (warning)))))
 '(fringe ((t (:background "white"))))
 '(minibuffer-prompt ((t (:weight bold))))

 ;; outline
 '(outline-1 ((t nil)))
 '(outline-2 ((t nil)))
 '(outline-3 ((t nil)))
 '(outline-4 ((t nil)))
 '(outline-5 ((t nil)))
 '(outline-6 ((t nil)))
 '(outline-7 ((t nil)))
 '(outline-8 ((t nil)))

 ;; org
 '(org-block ((t nil)))

 ;; LaTeX
 '(font-latex-sedate-face ((t (:inherit (font-lock-keyword-face)))))
 '(font-latex-string-face ((t (:inherit (font-lock-string-face)))))
 '(font-latex-math-face ((t (:background "#FFF0FF"))))
 '(font-latex-script-char-face ((t nil)))

 ;; Dired
 '(dired-directory ((t (:weight bold))))

 ;; Haskell
 '(haskell-constructor-face ((t nil)))

 ;; eglot
 '(eglot-highlight-symbol-face ((t (:inherit (region)))))

 ;; which-key
 '(which-key-separator-face ((t nil))))

(provide-theme 'fourma)
