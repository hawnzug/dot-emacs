(deftheme fourma
  "Created 2020-10-25.")

(custom-theme-set-faces
 'fourma
 '(cursor ((t (:background "black"))))
 '(highlight ((t (:background "#add9ff"))))
 '(hl-line ((t nil)))
 '(region ((t (:background "#add9ff"))))
 '(warning ((t (:background "#FF6666"))))
 '(secondary-selection ((t (:background "light yellow"))))
 '(trailing-whitespace ((t (:background "red"))))
 '(font-lock-builtin-face ((t (:slant italic))))
 '(font-lock-comment-delimiter-face ((t (:inherit (font-lock-comment-face)))))
 '(font-lock-comment-face ((t (:background "#c1eec1"))))
 '(font-lock-constant-face ((t nil)))
 '(font-lock-doc-face ((t (:inherit (font-lock-comment-face)))))
 '(font-lock-function-name-face ((t nil)))
 '(font-lock-keyword-face ((t (:slant italic))))
 '(font-lock-negation-char-face ((t nil)))
 '(font-lock-preprocessor-face ((t (:inherit (font-lock-builtin-face)))))
 '(font-lock-regexp-grouping-backslash ((t nil)))
 '(font-lock-regexp-grouping-construct ((t nil)))
 '(font-lock-string-face ((t nil)))
 '(font-lock-type-face ((t nil)))
 '(font-lock-variable-name-face ((t nil)))
 '(font-lock-warning-face ((t (:inherit (warning)))))
 '(fringe ((t (:background "white"))))
 '(minibuffer-prompt ((t (:weight bold))))
 '(link ((t (:underline t :foreground "#0070CE"))))

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
 '(org-drawer ((t (:foreground "#BBBBBB"))))
 '(org-todo ((t (:background "#ff7073"))))
 '(org-block ((t (:inherit fixed-pitch))))

 ;; LaTeX
 '(font-latex-sedate-face ((t (:inherit (font-lock-keyword-face)))))
 '(font-latex-string-face ((t (:inherit (font-lock-string-face)))))
 '(font-latex-math-face ((t nil)))
 '(font-latex-script-char-face ((t nil)))

 ;; Dired
 '(dired-directory ((t (:weight bold))))

 ;; Company
 '(company-tooltip ((t (:background "#f0f0f0"))))
 '(company-tooltip-selection ((t (:inherit region))))
 '(company-tooltip-common ((t (:foreground "#707070"))))
 '(company-tooltip-annotation ((t (:foreground "#999999"))))
 '(company-tooltip-scrollbar-thumb ((t (:background "#666666"))))
 '(company-tooltip-scrollbar-track ((t (:background "#f6f6f6"))))

 ;; Haskell
 '(haskell-constructor-face ((t nil)))
 '(haskell-keyword-face ((t (:family "Monoflow"))))
 '(haskell-pragma-face ((t (:family "Monoflow"))))

 ;; Agda
 '(agda2-highlight-module-face ((t nil)))
 '(agda2-highlight-keyword-face ((t (:family "Monoflow"))))
 '(agda2-highlight-symbol-face ((t (:foreground "#555555" :weight light))))
 '(agda2-highlight-primitive-face ((t nil)))
 '(agda2-highlight-primitive-type-face ((t nil)))
 '(agda2-highlight-number-face ((t nil)))
 '(agda2-highlight-bound-variable-face ((t (:background "#FFF4F4"))))
 '(agda2-highlight-datatype-face ((t (:background "#F4F4FF"))))
 '(agda2-highlight-catchall-clause-face ((t nil)))
 '(agda2-highlight-function-face ((t (:inherit (agda2-highlight-datatype-face)))))
 '(agda2-highlight-operator-face ((t (:inherit (agda2-highlight-datatype-face)))))
 '(agda2-highlight-inductive-constructor-face ((t (:inherit (agda2-highlight-datatype-face)))))

 ;; eglot
 '(eglot-highlight-symbol-face ((t (:inherit (region)))))

 ;; which-key
 '(which-key-separator-face ((t nil))))

(provide-theme 'fourma)
