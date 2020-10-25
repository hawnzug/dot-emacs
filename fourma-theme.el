(deftheme fourma
  "Created 2020-10-25.")

; #800000 #802020 #804040 #806060 #808080
; #900000 #902020 #904040 #906060 #908080
; #A00000 #A02020 #A04040 #A06060 #A08080
; #B00000 #B02020 #B04040 #B06060 #B08080
; #C00000 #C02020 #C04040 #C06060 #C08080
; #D00000 #D02020 #D04040 #D06060 #D08080
; #E00000 #E02020 #E04040 #E06060 #E08080
; #F00000 #F02020 #F04040 #F06060 #F08080

(custom-theme-set-faces
 'fourma
 '(default ((t (:family "Iosevka Curly Slab"))))
 '(cursor ((t (:background "black"))))
 '(fixed-pitch ((t (:family "Iosevka Curly Slab"))))
 '(variable-pitch ((t (:family "Merriweather"))))
 '(highlight ((t (:background "grey"))))
 '(hl-line ((t nil)))
 '(region ((t (:background "Skyblue1"))))
 '(secondary-selection ((t (:background "light yellow"))))
 '(trailing-whitespace ((t (:background "red"))))
 '(font-lock-builtin-face ((t (:foreground "#D00000"))))
 '(font-lock-comment-delimiter-face ((t (:foreground "grey"))))
 '(font-lock-comment-face ((t (:inherit (font-lock-string-face)))))
 '(font-lock-constant-face ((t (:foreground "#D00000"))))
 '(font-lock-doc-face ((t (:inherit (font-lock-string-face)))))
 '(font-lock-function-name-face ((t (:inherit (default)))))
 '(font-lock-keyword-face ((t (:foreground "#D00000"))))
 '(font-lock-negation-char-face ((t nil)))
 '(font-lock-preprocessor-face ((t (:inherit (font-lock-builtin-face)))))
 '(font-lock-regexp-grouping-backslash ((t (:inherit (bold)))))
 '(font-lock-regexp-grouping-construct ((t (:inherit (bold)))))
 '(font-lock-string-face ((t (:foreground "DarkOliveGreen4"))))
 '(font-lock-type-face ((t (:foreground "SteelBlue4"))))
 '(font-lock-variable-name-face ((t (:foreground "#D00000"))))
 '(font-lock-warning-face ((t (:inherit (error)))))
 '(fringe ((t (:background "white"))))

 ;; LaTeX
 '(font-latex-sedate-face ((t (:inherit (font-lock-keyword-face)))))
 '(font-latex-string-face ((t (:inherit (font-lock-string-face)))))
 '(font-latex-math-face ((t (:inherit (font-lock-type-face)))))
 '(font-latex-script-char-face ((t nil)))

 ;; Dired
 '(dired-directory ((t (:inherit (font-lock-type-face)))))
 )

(provide-theme 'fourma)
