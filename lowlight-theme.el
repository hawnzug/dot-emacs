;;; lowlight-theme.el --- hawnzug's light custom theme
;;; -*- lexical-binding: t -*-

;;; Commentary:

;;; Code:

(deftheme lowlight)

(custom-theme-set-faces
   'lowlight
   '(default ((t (:background "white" :foreground "black" :font "Sarasa Mono SC-12"))))
   '(mode-line ((t (:background "white"))))
   '(fringe ((t (:background "white"))))
   '(Mode-line-inactive ((t (:inherit mode-line))))
   '(region ((t (:background "#08D9D6"))))
   '(highlight ((t (:background "forest green"))))
   '(link ((t (:foreground "royal blue" :underline t))))
   '(show-paren-match ((t (:foreground "black" :background "cyan"))))
   '(show-paren-mismatch ((t (:background "firebrick1"))))
   '(ivy-current-match ((t (:background "aquamarine"))))
   '(font-lock-type-face ((t (:foreground "firebrick3"))))
   '(font-lock-builtin-face ((t (:foreground "DodgerBlue4"))))
   '(font-lock-keyword-face ((t (:foreground "DodgerBlue4"))))
   '(font-lock-comment-face ((t (:foreground "grey50"))))
   '(font-lock-function-name-face ((t (:foreground "black"))))
   '(font-lock-variable-name-face ((t (:foreground "black"))))
   '(font-lock-string-face ((t (:foreground "#7B14A3"))))
   '(diff-added ((t (:inherit diff-changed :background "light green"))))
   '(diff-changed ((t (:background "light steel blue"))))
   '(diff-indicator-added ((t (:inherit diff-indicator-changed))))
   '(diff-indicator-changed ((t (:weight bold))))
   '(diff-indicator-removed ((t (:inherit diff-indicator-changed))))
   '(diff-removed ((t (:inherit diff-changed :background "sandy brown"))))
   '(hl-line ((t (:background "#f0f0f1"))))
   '(hl-paren-face ((t (:weight bold))) t)
   '(minibuffer-prompt ((t (:foreground "#0184bc" :box (:line-width -1 :style released-button) :weight bold))))
   '(cfw:face-title ((t (:weight bold :height 2.5))))
   '(cfw:face-header ((t :weight bold)))
   '(cfw:face-sunday ((t :inherit cfw:face-header)))
   '(cfw:face-saturday ((t :inherit cfw:face-header)))
   '(cfw:face-holiday ((t :inherit cfw:face-header)))
   '(cfw:face-grid ((t :foreground "black")))
   '(cfw:face-default-content ((t :foreground "#bfebbf")))
   '(cfw:face-periods ((t :foreground "cyan")))
   '(cfw:face-day-title ((t)))
   '(cfw:face-today-title ((t :weight bold :inherit cfw:face-day-title)))
   '(cfw:face-default-day ((t :weight bold :inherit cfw:face-day-title)))
   '(cfw:face-annotation ((t :foreground "RosyBrown" :inherit cfw:face-day-title)))
   '(cfw:face-disable ((t :foreground "DarkGray" :inherit cfw:face-day-title)))
   '(cfw:face-today ((t :weight bold)))
   '(cfw:face-select ((t :background "aquamarine")))
   '(cfw:face-toolbar ((t)))
   '(cfw:face-toolbar-button-off ((t :foreground "Gray10" :weight bold)))
   '(cfw:face-toolbar-button-on ((t :foreground "Gray50" :weight bold)))
   '(outline-1 ((t (:foreground "black" :weight bold))))
   '(outline-2 ((t (:foreground "black" :weight bold))))
   '(outline-3 ((t (:foreground "black" :weight bold))))
   '(outline-4 ((t (:foreground "black" :weight bold))))
   '(outline-5 ((t (:foreground "black" :weight bold))))
   '(outline-6 ((t (:foreground "black" :weight bold))))
   '(outline-7 ((t (:foreground "black" :weight bold))))
   '(outline-8 ((t (:foreground "black" :weight bold))))
   '(magit-section-heading ((t (:foreground "DodgerBlue4" :weight bold))))
   '(org-scheduled-previously ((t (:foreground "#E84545"))))
   '(org-agenda-structure ((t (:foreground "DodgerBlue4" :weight bold))))
   '(org-agenda-date ((t (:foreground "DodgerBlue4"))))
   '(org-agenda-date-weekend ((t (:inherit org-agenda-date))))
   '(org-agenda-date-today ((t (:inherit org-agenda-date :weight bold))))
   '(org-agenda-done ((t (:foreground "black"))))
   '(org-scheduled-today ((t (:foreground "black"))))
   '(org-agenda-clocking ((t ())))
   '(org-time-grid ((t (:foreground "grey60"))))
   '(org-date ((t (:foreground "MediumPurple3" :underline t))))
   '(org-todo ((t (:foreground "#FF2E63" :weight bold))))
   '(org-done ((t (:foreground "#00A388" :weight bold))))
   '(org-block ((t (:foreground "black"))))
   '(org-level-1 ((t (:inherit outline-1))))
   '(org-level-2 ((t (:inherit outline-2))))
   '(org-level-3 ((t (:inherit outline-3))))
   '(org-level-4 ((t (:inherit outline-4))))
   '(org-level-5 ((t (:inherit outline-5))))
   '(org-level-6 ((t (:inherit outline-6))))
   '(org-level-7 ((t (:inherit outline-7))))
   '(org-level-8 ((t (:inherit outline-8))))
   '(org-tag ((t (:weight bold)))))

(provide-theme 'lowlight)

;; Local Variables:
;; no-byte-compile: t
;; End:

;;; lowlight-theme.el ends here
