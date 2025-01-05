(deftheme fourma
  "Created 2020-10-25.")

;; https://colorjs.io/apps/picker/jzczhz
;; (new Color("color(jzczhz jz cz hz)")).to("srgb").toString({format: "hex"})
;; rgb     jz   cz   hz
;; #ffcecd 0.2  0.04 30
;; #f1e5b6 0.2  0.04 90
;; #b8face 0.2  0.04 150
;; #a6f6fb 0.2  0.04 210
;; #d7ddff 0.2  0.04 270
;; #ffc8f7 0.2  0.04 270
;; #920021 0.08 0.16 30
;; #764a00 0.08 0.16 90
;; #007639 0.08 0.16 150
;; #00696f 0.08 0.16 210
;; #4500c6 0.08 0.16 270
;; #7b006c 0.08 0.16 330

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

(let* ((font-slab "Iosevka Curly Slab 24")
       (fg-default "#000000")
       (background "#FFFFFF")
       (background-light "#FDFFFA")
       (background-dark "#F8F8F8")
       (background-dark-1 "#E0E0E0")

       (bg-grey "#D8E5E2")
       (bg-red "#FF8870")
       (bg-orange "#FFA247")
       (bg-yellow "#FFDD5a")
       (bg-green "#D4FFAD")
       (bg-sky-blue "#AAEAF4")
       (bg-blue "#84A9FF")
       (bg-purple "#AD87FF")
       (bg-pink "#F7AEFF")

       (bg-selection bg-blue)
       (bg-highlight bg-pink)
       (bg-info bg-yellow)
       (bg-error "#FF451E")

       (fg-0 "#B33636")
       (fg-1 "#B37636")
       (fg-2 "#8DB336")
       (fg-3 "#36B351")
       (fg-4 "#36B3A0")
       (fg-5 "#366CB3")
       (fg-6 "#9336B3")
       (fg-7 "#B3369C")

       (fg-grey "#5F5E54"))
  (custom-theme-set-faces
   'fourma
   `(default ((t (:background ,background))))
   `(fringe ((t (:inherit default))))
   `(window-divider ((t (:foreground ,fg-default))))
   `(window-divider-first-pixel ((t (:foreground ,fg-default))))
   `(window-divider-last-pixel ((t (:foreground ,fg-default))))
   `(cursor ((t (:background ,fg-default))))
   `(highlight ((t (:background ,bg-highlight))))
   `(show-paren-match ((t (:background ,bg-info))))
   `(warning ((t (:background ,bg-error))))
   `(trailing-whitespace ((t (:inherit warning))))
   `(region ((t (:background ,bg-selection))))
   `(secondary-selection ((t (:background ,bg-selection))))
   '(minibuffer-prompt ((t (:weight bold))))
   `(link ((t (:underline t :foreground ,fg-5))))
   `(hl-line ((t (:background ,background-dark))))
   `(line-number ((t (:foreground "#999999" :background ,background-dark :weight light))))
   `(line-number-current-line ((t (:foreground "#444" :background "#FFFFFF" :weight light))))

   `(font-lock-comment-face ((t (:background ,background-dark :weight light :font ,font-slab))))
   `(font-lock-comment-delimiter-face ((t (:inherit (font-lock-comment-face)))))
   '(font-lock-doc-face ((t (:inherit (font-lock-comment-face)))))
   `(font-lock-builtin-face ((t (:weight light :font ,font-slab))))
   `(font-lock-constant-face ((t (:foreground ,fg-1))))
   `(font-lock-keyword-face ((t (:inherit font-lock-builtin-face))))
   `(font-lock-function-name-face ((t (:foreground ,fg-0))))
   '(font-lock-negation-char-face ((t nil)))
   '(font-lock-preprocessor-face ((t (:inherit (font-lock-builtin-face)))))
   '(font-lock-regexp-grouping-backslash ((t nil)))
   '(font-lock-regexp-grouping-construct ((t nil)))
   `(font-lock-string-face ((t (:foreground ,fg-7))))
   `(font-lock-type-face ((t (:foreground ,fg-5))))
   `(font-lock-variable-name-face ((t (:foreground ,fg-6))))
   '(font-lock-warning-face ((t (:inherit (warning)))))

   ;; orderless
   `(orderless-match-face-0 ((t (:slant italic :background ,bg-blue))))
   `(orderless-match-face-1 ((t (:slant italic :background ,bg-purple))))
   `(orderless-match-face-2 ((t (:slant italic :background ,bg-green))))
   `(orderless-match-face-3 ((t (:slant italic :background ,bg-red))))

   ;; magit
   `(magit-section-heading ((t (:foreground ,fg-5 :weight bold))))
   `(magit-section-highlight ((t (:background "white"))))
   `(magit-difg-hunk-heading ((t (:background ,background-dark :overline t :foreground ,fg-grey))))
   `(magit-difg-hunk-heading-highlight ((t (:foreground "black" :weight bold))))

   ;; consult
   `(consult-line-number-wrapped ((t (:background ,bg-pink))))

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
   `(org-level-1 ((t (:weight bold))))
   `(org-level-2 ((t (:inherit org-level-1))))
   `(org-level-3 ((t (:inherit org-level-1))))
   `(org-level-4 ((t (:inherit org-level-1))))
   `(org-level-5 ((t (:inherit org-level-1))))
   `(org-level-6 ((t (:inherit org-level-1))))
   `(org-level-7 ((t (:inherit org-level-1))))
   `(org-level-8 ((t (:inherit org-level-1))))
   `(org-hide ((t (:foreground ,background))))
   `(org-drawer ((t (:inherit shadow))))
   `(org-todo ((t (:background ,background-dark :foreground "#555555" :weight ultra-light))))
   `(org-done ((t (:background ,background-dark :foreground "#AAAAAA" :strike-through t :weight ultra-light))))
   `(org-date ((t (:underline t :foreground ,fg-1))))
   `(org-document-title ((t (:weight bold :foreground ,fg-5))))
   `(org-document-info ((t (:foreground ,fg-4))))
   `(org-block ((t (:inherit fixed-pitch))))
   `(org-indent ((t (:inherit (org-hide fixed-pitch)))))
   `(org-table ((t (:inherit fixed-pitch))))
   `(org-tag ((t (:inherit fixed-pitch))))
   `(org-verbatim ((t (:inherit fixed-pitch :foreground ,fg-7))))
   `(org-agenda-structure ((t (:foreground ,fg-6))))
   `(org-scheduled ((t (:foreground ,fg-grey))))
   `(org-scheduled-today ((t (:foreground ,fg-1))))

   ;; Agda
   `(agda2-highlight-function-face ((t nil)))
   `(agda2-highlight-operator-face ((t nil)))
   `(agda2-highlight-datatype-face ((t nil)))
   `(agda2-highlight-module-face ((t nil)))
   `(agda2-highlight-record-face ((t nil)))
   `(agda2-highlight-inductive-constructor-face ((t nil)))
   `(agda2-highlight-keyword-face ((t (:inherit font-lock-builtin-face))))
   `(agda2-highlight-symbol-face ((t (:inherit font-lock-builtin-face))))
   `(agda2-highlight-primitive-face ((t (:inherit font-lock-builtin-face))))
   `(agda2-highlight-primitive-type-face ((t (:inherit font-lock-builtin-face))))
   `(agda2-highlight-bound-variable-face ((t (:slant italic))))
   `(agda2-highlight-field-face ((t nil)))
   `(agda2-highlight-postulate-face ((t nil)))

   ;; LaTeX
   '(font-latex-sedate-face ((t (:inherit (font-lock-keyword-face)))))
   '(font-latex-string-face ((t (:inherit (font-lock-string-face)))))
   '(font-latex-math-face ((t nil)))
   '(font-latex-script-char-face ((t nil)))

   ;; Dired
   `(dired-directory ((t (:weight bold :foreground ,fg-6))))

   ;; Haskell
   '(haskell-constructor-face ((t nil)))
   '(haskell-keyword-face ((t (:family "Monoflow"))))
   '(haskell-pragma-face ((t (:family "Monoflow"))))

   `(header-line ((t (:box t))))

   `(mode-line ((t (:box (:color "black" :line-width 1) :background ,background-light))))
   `(mode-line-inactive ((t (:box "#AAAAAA" :foreground ,fg-grey))))

   ;; tab-bar
   `(tab-bar ((t (:background ,background :height 0.8 :underline (:color "#000000" :position 0)))))
   `(tab-bar-tab ((t (:background "#F0F0F0" :weight semi-bold))))
   `(tab-bar-tab-inactive ((t (:background ,background :foreground ,fg-grey :weight light))))

   ;; eglot
   '(eglot-highlight-symbol-face ((t (:inherit (region)))))

   ;; which-key
   '(which-key-separator-face ((t nil)))))



(provide-theme 'fourma)
