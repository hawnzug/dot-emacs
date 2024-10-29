;; (setq package-enable-at-startup nil)
(setq frame-inhibit-implied-resize t)

(scroll-bar-mode -1)
(horizontal-scroll-bar-mode -1)
(tool-bar-mode -1)
(tooltip-mode -1)
(menu-bar-mode -1)

(startup-redirect-eln-cache
 (convert-standard-filename
  (expand-file-name  "var/eln-cache/" user-emacs-directory)))
