(deftheme fourma
  "Created 2020-10-25.")

;; https://colorjs.io/apps/picker/jzczhz
;; (new Color("color(jzczhz jz cz hz)")).to("srgb").toString({format: "hex"})
;; rgb     jz   cz   hz
;; #f1e5b6 0.2  0.04 90
;; #b8face 0.2  0.04 150
;; #a9f5fe 0.2  0.04 215
;; #a44148 0.1  0.08 30
;; #0673a4 0.1  0.08 240
;; #973787 0.1  0.08 330
;; #ec8ba0 0.16 0.06 15
;; #c4b373 0.16 0.06 90
;; #81cc8b 0.16 0.06 140
;; #67c1e1 0.16 0.06 230
;; #7d383c 0.08 0.06 30
;; #1d5b7c 0.08 0.06 240
;; #743368 0.08 0.06 330

(custom-theme-set-faces
 'fourma
 '(cursor ((t (:background "black"))))
 '(highlight ((t (:background "#67c1e1"))))
 '(hl-line ((t nil)))
 '(region ((t (:background "#67c1e1"))))
 '(warning ((t (:background "#FF6666"))))
 '(secondary-selection ((t (:background "light yellow"))))
 '(trailing-whitespace ((t (:background "red"))))
 '(show-paren-match ((t (:background "#c4b373"))))
 '(font-lock-builtin-face ((t (:slant italic))))
 '(font-lock-comment-delimiter-face ((t (:inherit (font-lock-comment-face)))))
 '(font-lock-comment-face ((t (:background "#81cc8b"))))
 '(font-lock-constant-face ((t nil)))
 '(font-lock-doc-face ((t (:inherit (font-lock-comment-face)))))
 '(font-lock-function-name-face ((t (:foreground "#7d383c"))))
 '(font-lock-keyword-face ((t (:slant italic))))
 '(font-lock-negation-char-face ((t nil)))
 '(font-lock-preprocessor-face ((t (:inherit (font-lock-builtin-face)))))
 '(font-lock-regexp-grouping-backslash ((t nil)))
 '(font-lock-regexp-grouping-construct ((t nil)))
 '(font-lock-string-face ((t (:foreground "#743368"))))
 '(font-lock-type-face ((t nil)))
 '(font-lock-variable-name-face ((t nil)))
 '(font-lock-warning-face ((t (:inherit (warning)))))
 '(fringe ((t (:background "white"))))
 '(minibuffer-prompt ((t (:weight bold))))
 '(link ((t (:underline t :foreground "#1d5b7c"))))

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
 '(org-drawer ((t (:inherit shadow))))
 '(org-todo ((t (:background "#ec8ba0"))))
 '(org-done ((t (:inherit shadow))))
 '(org-date ((t (:underline t :foreground "#1d5b7c"))))
 '(org-document-title ((t (:weight bold :foreground "#1d5b7c"))))
 '(org-document-info ((t (:foreground "#1d5b7c"))))
 '(org-block ((t (:inherit fixed-pitch))))
 '(org-indent ((t (:inherit (org-hide fixed-pitch)))))
 '(org-table ((t (:inherit fixed-pitch))))
 '(org-tag ((t (:inherit fixed-pitch))))
 '(org-verbatim ((t (:inherit fixed-pitch :foreground "#7d383c"))))

 ;; LaTeX
 '(font-latex-sedate-face ((t (:inherit (font-lock-keyword-face)))))
 '(font-latex-string-face ((t (:inherit (font-lock-string-face)))))
 '(font-latex-math-face ((t nil)))
 '(font-latex-script-char-face ((t nil)))

 ;; Dired
 '(dired-directory ((t (:weight bold :foreground "#1d5b7c"))))

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

 ;; tab-bar
 '(tab-bar ((t (:background "#dddddd"))))
 '(tab-bar-tab ((t (:weight bold :background "#ffffff"))))
 '(tab-bar-tab-inactive ((t (:foreground "#999999"))))

 ;; eglot
 '(eglot-highlight-symbol-face ((t (:inherit (region)))))

 ;; which-key
 '(which-key-separator-face ((t nil))))

(provide-theme 'fourma)
