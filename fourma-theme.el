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

(let* ((background "#F2EDD5")
       (background-light "#FCF7DF")
       (background-dark "#E8E3CB")
       (bg-grey "#D8E5E2")
       (bg-red "#FF8870")
       (bg-orange "#FFA247")
       (bg-yellow "#FFDD5a")
       (bg-green "#D4FFAD")
       (bg-sky-blue "#AAEAF4")
       (bg-blue "#84A9FF")
       (bg-purple "#AD87FF")
       (bg-pink "#F7AEFF")

       (fg-grey "#5F5E54")
       (fg-red "#820000")
       (fg-brown "#573F00")
       (fg-green "#237142")
       (fg-sky-blue "#3E6366")
       (fg-blue "#00327A")
       (fg-purple "#7B006C")

       (bg-selection bg-orange)
       (bg-error "#FF451E"))
  (custom-theme-set-faces
   'fourma
   `(default ((t (:background ,background))))
   `(fringe ((t (:inherit default))))
   `(cursor ((t (:background ,fg-green))))
   `(hl-line ((t (:background "white"))))
   `(highlight ((t (:background ,bg-yellow))))
   `(show-paren-match ((t (:inherit highlight))))
   `(warning ((t (:background ,bg-error))))
   `(trailing-whitespace ((t (:inherit warning))))
   `(region ((t (:background ,bg-selection))))
   `(secondary-selection ((t (:background ,bg-selection))))
   '(minibuffer-prompt ((t (:weight bold))))
   `(link ((t (:underline t :foreground ,fg-blue))))

   `(font-lock-comment-face ((t (:background ,bg-grey))))
   `(font-lock-comment-delimiter-face ((t (:inherit (font-lock-comment-face)))))
   '(font-lock-doc-face ((t (:inherit (font-lock-comment-face)))))
   `(font-lock-builtin-face ((t (:weight light))))
   `(font-lock-constant-face ((t (:foreground ,fg-red))))
   `(font-lock-keyword-face ((t (:inherit font-lock-builtin-face))))
   `(font-lock-function-name-face ((t (:foreground ,fg-sky-blue))))
   '(font-lock-negation-char-face ((t nil)))
   '(font-lock-preprocessor-face ((t (:inherit (font-lock-builtin-face)))))
   '(font-lock-regexp-grouping-backslash ((t nil)))
   '(font-lock-regexp-grouping-construct ((t nil)))
   `(font-lock-string-face ((t (:foreground ,fg-purple))))
   '(font-lock-type-face ((t nil)))
   '(font-lock-variable-name-face ((t nil)))
   '(font-lock-warning-face ((t (:inherit (warning)))))

   ;; orderless
   `(orderless-match-face-0 ((t (:slant italic :background ,bg-blue))))
   `(orderless-match-face-1 ((t (:slant italic :background ,bg-purple))))
   `(orderless-match-face-2 ((t (:slant italic :background ,bg-green))))
   `(orderless-match-face-3 ((t (:slant italic :background ,bg-red))))

   ;; magit
   `(magit-section-heading ((t (:foreground ,fg-blue :weight bold))))
   `(magit-section-highlight ((t (:background "white"))))
   `(magit-diff-hunk-heading ((t (:background ,bg-grey :foreground ,fg-grey))))
   `(magit-diff-hunk-heading-highlight ((t (:foreground "black" :weight bold))))

   ;; consult
   `(consult-line-number-wrapped ((t (:background ,bg-pink))))

   ;; rime
   `(rime-default-face ((t (:background ,bg-green))))
   `(rime-candidate-num-face ((t (:foreground ,fg-red :inherit rime-default-face))))
   `(rime-highlight-candidate-face ((t (:inherit highlight))))
   `(rime-comment-face ((t (:foreground ,fg-grey :inherit rime-default-face))))
   `(rime-code-face ((t (:inherit rime-default-face))))

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
   `(org-hide ((t (:foreground ,background))))
   `(org-drawer ((t (:inherit shadow))))
   `(org-todo ((t (:background ,bg-red))))
   `(org-done ((t (:background ,bg-sky-blue))))
   `(org-date ((t (:underline t :foreground ,fg-brown))))
   `(org-document-title ((t (:weight bold :foreground ,fg-blue))))
   `(org-document-info ((t (:foreground ,fg-blue))))
   `(org-block ((t (:inherit fixed-pitch))))
   `(org-indent ((t (:inherit (org-hide fixed-pitch)))))
   `(org-table ((t (:inherit fixed-pitch))))
   `(org-tag ((t (:inherit fixed-pitch))))
   `(org-verbatim ((t (:inherit fixed-pitch :foreground ,fg-brown))))
   `(org-agenda-structure ((t (:foreground ,fg-blue))))
   `(org-scheduled ((t (:foreground ,fg-grey))))
   `(org-scheduled-today ((t (:foreground ,fg-red))))

   ;; Agda
   `(agda2-highlight-function-face ((t (:inherit font-lock-function-name-face))))
   `(agda2-highlight-operator-face ((t (:inherit font-lock-function-name-face))))
   `(agda2-highlight-datatype-face ((t (:inherit font-lock-function-name-face))))
   `(agda2-highlight-module-face ((t (:foreground ,fg-purple))))
   `(agda2-highlight-keyword-face ((t (:inherit font-lock-builtin-face))))
   `(agda2-highlight-primitive-face ((t (:inherit font-lock-builtin-face))))
   `(agda2-highlight-primitive-type-face ((t (:inherit font-lock-builtin-face))))
   `(agda2-highlight-bound-variable-face ((t (:foreground ,fg-blue))))
   `(agda2-highlight-postulate-face ((t (:background ,bg-red))))

   ;; LaTeX
   '(font-latex-sedate-face ((t (:inherit (font-lock-keyword-face)))))
   '(font-latex-string-face ((t (:inherit (font-lock-string-face)))))
   '(font-latex-math-face ((t nil)))
   '(font-latex-script-char-face ((t nil)))

   ;; Dired
   `(dired-directory ((t (:weight bold :foreground ,fg-blue))))

   ;; Haskell
   '(haskell-constructor-face ((t nil)))
   '(haskell-keyword-face ((t (:family "Monoflow"))))
   '(haskell-pragma-face ((t (:family "Monoflow"))))

   `(header-line ((t (:box t))))

   `(mode-line ((t (:box (:color "black" :line-width 1) :background ,background-light))))
   `(mode-line-inactive ((t (:box "#AAAAAA" :foreground ,fg-grey))))

   ;; tab-bar
   `(tab-bar ((t (:background ,background-light :underline (:color "black" :position t)))))
   `(tab-bar-tab ((t (:background ,background :slant italic))))
   `(tab-bar-tab-inactive ((t (:foreground ,fg-grey :weight light))))

   ;; eglot
   '(eglot-highlight-symbol-face ((t (:inherit (region)))))

   ;; which-key
   '(which-key-separator-face ((t nil)))))



(provide-theme 'fourma)
