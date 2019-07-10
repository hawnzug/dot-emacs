;;; lowlight-theme.el --- hawnzug's light custom theme
;;; -*- lexical-binding: t -*-

;;; Commentary:

;;; Code:

(deftheme lowlight)

(let ((color-bg "#F0F0F0")
      (color-fg "#000000")

      (brown-50    "#EFEBE9") (gray-50    "#FAFAFA") (blue-gray-50     "#ECEFF1")
      (brown-100   "#D7CCC8") (gray-100   "#F5F5F5") (blue-gray-100    "#CFD8DC")
      (brown-200   "#BCAAA4") (gray-200   "#EEEEEE") (blue-gray-200    "#B0BEC5")
      (brown-300   "#A1887F") (gray-300   "#E0E0E0") (blue-gray-300    "#90A4AE")
      (brown-400   "#8D6E63") (gray-400   "#BDBDBD") (blue-gray-400    "#78909C")
      (brown-500   "#795548") (gray-500   "#9E9E9E") (blue-gray-500    "#607D8B")
      (brown-600   "#6D4C41") (gray-600   "#757575") (blue-gray-600    "#546E7A")
      (brown-700   "#5D4037") (gray-700   "#616161") (blue-gray-700    "#455A64")
      (brown-800   "#4E342E") (gray-800   "#424242") (blue-gray-800    "#37474F")
      (brown-900   "#3E2723") (gray-900   "#212121") (blue-gray-900    "#263238")

      (red-50      "#FFEBEE") (pink-50    "#FCE4EC") (purple-50        "#F3E5F5") (deep-purple-50   "#EDE7F6")
      (red-100     "#FFCDD2") (pink-100   "#F8BBD0") (purple-100       "#E1BEE7") (deep-purple-100  "#D1C4E9")
      (red-200     "#EF9A9A") (pink-200   "#F48FB1") (purple-200       "#CE93D8") (deep-purple-200  "#B39DDB")
      (red-300     "#E57373") (pink-300   "#F06292") (purple-300       "#BA68C8") (deep-purple-300  "#9575CD")
      (red-400     "#EF5350") (pink-400   "#EC407A") (purple-400       "#AB47BC") (deep-purple-400  "#7E57C2")
      (red-500     "#F44336") (pink-500   "#E91E63") (purple-500       "#9C27B0") (deep-purple-500  "#673AB7")
      (red-600     "#E53935") (pink-600   "#D81B60") (purple-600       "#8E24AA") (deep-purple-600  "#5E35B1")
      (red-700     "#D32F2F") (pink-700   "#C2185B") (purple-700       "#7B1FA2") (deep-purple-700  "#512DA8")
      (red-800     "#C62828") (pink-800   "#AD1457") (purple-800       "#6A1B9A") (deep-purple-800  "#4527A0")
      (red-900     "#B71C1C") (pink-900   "#880E4F") (purple-900       "#4A148C") (deep-purple-900  "#311B92")
      (red-A100    "#FF8A80") (pink-A100  "#FF80AB") (purple-A100      "#EA80FC") (deep-purple-A100 "#B388FF")
      (red-A200    "#FF5252") (pink-A200  "#FF4081") (purple-A200      "#E040FB") (deep-purple-A200 "#7C4DFF")
      (red-A400    "#FF1744") (pink-A400  "#F50057") (purple-A400      "#D500F9") (deep-purple-A400 "#651FFF")
      (red-A700    "#D50000") (pink-A700  "#C51162") (purple-A700      "#AA00FF") (deep-purple-A700 "#6200EA")

      (indigo-50   "#E8EAF6") (blue-50    "#E3F2FD") (light-blue-50    "#E1F5FE") (cyan-50          "#E0F7FA")
      (indigo-100  "#C5CAE9") (blue-100   "#BBDEFB") (light-blue-100   "#B3E5FC") (cyan-100         "#B2EBF2")
      (indigo-200  "#9FA8DA") (blue-200   "#90CAF9") (light-blue-200   "#81D4FA") (cyan-200         "#80DEEA")
      (indigo-300  "#7986CB") (blue-300   "#64B5F6") (light-blue-300   "#4FC3F7") (cyan-300         "#4DD0E1")
      (indigo-400  "#5C6BC0") (blue-400   "#42A5F5") (light-blue-400   "#29B6F6") (cyan-400         "#26C6DA")
      (indigo-500  "#3F51B5") (blue-500   "#2196F3") (light-blue-500   "#03A9F4") (cyan-500         "#00BCD4")
      (indigo-600  "#3949AB") (blue-600   "#1E88E5") (light-blue-600   "#039BE5") (cyan-600         "#00ACC1")
      (indigo-700  "#303F9F") (blue-700   "#1976D2") (light-blue-700   "#0288D1") (cyan-700         "#0097A7")
      (indigo-800  "#283593") (blue-800   "#1565C0") (light-blue-800   "#0277BD") (cyan-800         "#00838F")
      (indigo-900  "#1A237E") (blue-900   "#0D47A1") (light-blue-900   "#01579B") (cyan-900         "#006064")
      (indigo-A100 "#8C9EFF") (blue-A100  "#82B1FF") (light-blue-A100  "#80D8FF") (cyan-A100        "#84FFFF")
      (indigo-A200 "#536DFE") (blue-A200  "#448AFF") (light-blue-A200  "#40C4FF") (cyan-A200        "#18FFFF")
      (indigo-A400 "#3D5AFE") (blue-A400  "#2979FF") (light-blue-A400  "#00B0FF") (cyan-A400        "#00E5FF")
      (indigo-A700 "#304FFE") (blue-A700  "#2962FF") (light-blue-A700  "#0091EA") (cyan-A700        "#00B8D4")

      (teal-50     "#E0F2F1") (green-50   "#E8F5E9") (light-Green-50   "#F1F8E9") (lime-50          "#F9FBE7")
      (teal-100    "#B2DFDB") (green-100  "#C8E6C9") (light-green-100  "#DCEDC8") (lime-100         "#F0F4C3")   
      (teal-200    "#80CBC4") (green-200  "#A5D6A7") (light-green-200  "#C5E1A5") (lime-200         "#E6EE9C")   
      (teal-300    "#4DB6AC") (green-300  "#81C784") (light-green-300  "#AED581") (lime-300         "#DCE775")   
      (teal-400    "#26A69A") (green-400  "#66BB6A") (light-green-400  "#9CCC65") (lime-400         "#D4E157")
      (teal-500    "#009688") (green-500  "#4CAF50") (light-green-500  "#8BC34A") (lime-500         "#CDDC39")   
      (teal-600    "#00897B") (green-600  "#43A047") (light-green-600  "#7CB342") (lime-600         "#C0CA33")   
      (teal-700    "#00796B") (green-700  "#388E3C") (light-green-700  "#689F38") (lime-700         "#AFB42B")   
      (teal-800    "#00695C") (green-800  "#2E7D32") (light-green-800  "#558B2F") (lime-800         "#9E9D24")   
      (teal-900    "#004D40") (green-900  "#1B5E20") (light-green-900  "#33691E") (lime-900         "#827717")   
      (teal-A100   "#A7FFEB") (green-A100 "#B9F6CA") (light-green-A100 "#CCFF90") (lime-A100        "#F4FF81")   
      (teal-A200   "#64FFDA") (green-A200 "#69F0AE") (light-green-A200 "#B2FF59") (lime-A200        "#EEFF41")   
      (teal-A400   "#1DE9B6") (green-A400 "#00E676") (light-green-A400 "#76FF03") (lime-A400        "#C6FF00")   
      (teal-A700   "#00BFA5") (green-A700 "#00C853") (light-green-A700 "#64DD17") (lime-A700        "#AEEA00")   

      (yellow-50   "#FFFDE7") (amber-50   "#FFF8E1") (orange-50        "#FFF3E0") (deep-orange-50   "#FBE9E7")
      (yellow-100  "#FFF9C4") (amber-100  "#FFECB3") (orange-100       "#FFE0B2") (deep-orange-100  "#FFCCBC")  
      (yellow-200  "#FFF59D") (amber-200  "#FFE082") (orange-200       "#FFCC80") (deep-orange-200  "#FFAB91")  
      (yellow-300  "#FFF176") (amber-300  "#FFD54F") (orange-300       "#FFB74D") (deep-orange-300  "#FF8A65")  
      (yellow-400  "#FFEE58") (amber-400  "#FFCA28") (orange-400       "#FFA726") (deep-orange-400  "#FF7043")  
      (yellow-500  "#FFEB3B") (amber-500  "#FFC107") (orange-500       "#FF9800") (deep-orange-500  "#FF5722")  
      (yellow-600  "#FDD835") (amber-600  "#FFB300") (orange-600       "#FB8C00") (deep-orange-600  "#F4511E")  
      (yellow-700  "#FBC02D") (amber-700  "#FFA000") (orange-700       "#F57C00") (deep-orange-700  "#E64A19")  
      (yellow-800  "#F9A825") (amber-800  "#FF8F00") (orange-800       "#EF6C00") (deep-orange-800  "#D84315")  
      (yellow-900  "#F57F17") (amber-900  "#FF6F00") (orange-900       "#E65100") (deep-orange-900  "#BF360C")  
      (yellow-A100 "#FFFF8D") (amber-A100 "#FFE57F") (orange-A100      "#FFD180") (deep-orange-A100 "#FF9E80")  
      (yellow-A200 "#FFFF00") (amber-A200 "#FFD740") (orange-A200      "#FFAB40") (deep-orange-A200 "#FF6E40")  
      (yellow-A400 "#FFEA00") (amber-A400 "#FFC400") (orange-A400      "#FF9100") (deep-orange-A400 "#FF3D00")  
      (yellow-A700 "#FFD600") (amber-A700 "#FFAB00") (orange-A700      "#FF6D00") (deep-orange-A700 "#DD2C00")
      )
  (custom-theme-set-faces
   'lowlight
   ;; `(default ((t (:background ,color-bg :foreground ,color-fg :font "Sarasa Term SC-14"))))
   `(default ((t (:background ,color-bg :foreground ,color-fg :font "Iosevka Term Slab-12"))))
   `(fringe ((t (:foreground ,indigo-500))))
   `(window-divider ((t (:foreground ,gray-500 :background ,gray-500))))
   `(line-number ((t (:foreground ,gray-500))))
   `(region ((t (:background ,light-blue-100))))
   `(highlight ((t (:background ,yellow-500))))
   `(isearch ((t (:background ,yellow-500))))
   `(link ((t (:foreground ,blue-700 :underline t))))
   `(show-paren-match ((t (:foreground ,color-fg :background ,yellow-500))))
   `(show-paren-mismatch ((t (:background ,red-700))))
   `(trailing-whitespace ((t (:background ,red-700))))

   `(hl-line ((t (:background ,gray-300))))
   `(line-number-current-line ((t (:background ,color-bg))))

   `(mode-line ((t (:foreground ,gray-800 :background ,gray-300 :overline ,gray-500 :underline ,gray-500 :font "Sarasa Term SC-12"))))
   `(mode-line-inactive ((t (:foreground ,gray-500 :background ,gray-300 :overline ,gray-500 :underline ,gray-500 :font "Sarasa Term SC-12"))))

   `(font-lock-type-face ((t (:foreground ,color-fg))))
   `(font-lock-builtin-face ((t (:foreground ,indigo-500))))
   `(font-lock-keyword-face ((t (:foreground ,indigo-500))))
   `(font-lock-comment-face ((t (:foreground ,gray-500))))
   `(font-lock-function-name-face ((t (:foreground ,color-fg))))
   `(font-lock-variable-name-face ((t (:foreground ,color-fg))))
   `(font-lock-string-face ((t (:foreground ,green-900))))
   `(font-lock-constant-face ((t (:foreground ,deep-orange-900))))

   `(ivy-current-match ((t (:background ,light-blue-500))))
   `(ivy-minibuffer-match-face-2 ((t (:background ,yellow-500))))
   `(ivy-minibuffer-match-face-3 ((t (:background ,yellow-500))))
   `(ivy-minibuffer-match-face-4 ((t (:background ,yellow-500))))

   `(ivy-posframe ((t (:background "#EBEBEB"))))
   `(ivy-posframe-border ((t (:background ,gray-500))))

   `(minibuffer-prompt ((t (:foreground ,indigo-800 :weight bold))))

   `(magit-section-heading ((t (:foreground ,indigo-800 :weight bold))))
   `(magit-diff-hunk-heading ((t (:foreground ,color-fg :background ,blue-gray-100 :weight bold))))
   `(magit-diff-hunk-heading-highlight ((t (:foreground ,color-bg :background ,light-blue-600 :weight bold))))
   `(magit-diff-context ((t (:foreground ,gray-500))))
   `(magit-diff-context-highlight ((t (:foreground ,gray-600 :background ,gray-100))))
   `(magit-diff-added ((t (:foreground ,green-600 :background ,green-50))))
   `(magit-diff-added-highlight ((t (:foreground ,green-700 :background ,green-100))))
   `(magit-diff-removed ((t (:foreground ,red-600 :background ,red-50))))
   `(magit-diff-removed-highlight ((t (:foreground ,red-700 :background ,red-100))))

   `(outline-1 ((t (:foreground ,color-fg))))
   `(outline-2 ((t (:foreground ,color-fg))))
   `(outline-3 ((t (:foreground ,color-fg))))
   `(outline-4 ((t (:foreground ,color-fg))))
   `(outline-5 ((t (:foreground ,color-fg))))
   `(outline-6 ((t (:foreground ,color-fg))))
   `(outline-7 ((t (:foreground ,color-fg))))
   `(outline-8 ((t (:foreground ,color-fg))))

   `(org-agenda-structure ((t (:foreground ,indigo-800 :weight bold))))
   `(org-agenda-date ((t (:foreground ,indigo-800))))
   `(org-agenda-date-weekend ((t (:inherit org-agenda-date))))
   `(org-agenda-date-today ((t (:inherit org-agenda-date :weight bold))))
   `(org-agenda-done ((t (:foreground ,color-fg))))
   `(org-scheduled-today ((t (:foreground ,color-fg))))
   '(org-agenda-clocking ((t ())))
   `(org-time-grid ((t (:foreground ,gray-300))))
   `(org-date ((t (:foreground ,indigo-300 :underline t))))
   `(org-todo ((t (:foreground ,red-500 :weight bold))))
   `(org-done ((t (:foreground ,green-500 :weight bold))))
   `(org-block ((t (:foreground ,color-fg))))
   `(org-table ((t (:foreground ,color-fg))))
   `(org-level-1 ((t (:inherit outline-1))))
   `(org-level-2 ((t (:inherit outline-2))))
   `(org-level-3 ((t (:inherit outline-3))))
   `(org-level-4 ((t (:inherit outline-4))))
   `(org-level-5 ((t (:inherit outline-5))))
   `(org-level-6 ((t (:inherit outline-6))))
   `(org-level-7 ((t (:inherit outline-7))))
   `(org-level-8 ((t (:inherit outline-8))))
   `(org-tag ((t (:weight bold))))))

(provide-theme 'lowlight)

;; Local Variables:
;; no-byte-compile: t
;; End:

;;; lowlight-theme.el ends here
