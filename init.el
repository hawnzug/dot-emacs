(load "~/.config/emacs/config")

(custom-set-variables
 ;; custom-set-variables was added by Custom.
 ;; If you edit it by hand, you could mess it up, so be careful.
 ;; Your init file should contain only one such instance.
 ;; If there is more than one, they won't work right.
 '(package-selected-packages
   '(eglot dante haskell-mode hide-mode-line wgrep which-key-posframe flycheck-posframe find-file-in-project olivetti ivy-bibtex terminal-here company-posframe flycheck-rust cargo rust-mode docker-tramp dockerfile-mode csv-mode doom-themes doom-modeline yaml-mode sml-mode org-plus-contrib imenu-list tuareg web-mode emmet-mode keyfreq org-tree-slide academic-phrases org-make-toc exec-path-from-shell xterm-color ivy-posframe avy fcitx ibuffer-vc dired-open symbol-overlay htmlize evil-matchit alert proof-general ivy-hydra general auctex all-the-icons-dired eshell-z esh-autosuggest org-bullets ob-ipython ccls flycheck all-the-icons-ivy counsel hydra which-key rainbow-delimiters evil-surround evil magit eyebrowse company-coq company use-package))
 '(safe-local-variable-values
   '((auto-save-visited-interval . 0)
     (dante-repl-command-line "cabal" "v2-repl" dante-target "--builddir=dist-newstyle/dante")
     (dired-omit-extensions ".vo" ".vok" ".vos" ".aux" ".glob")
     (org-babel-use-quick-and-dirty-noweb-expansion . t))))
(custom-set-faces
 ;; custom-set-faces was added by Custom.
 ;; If you edit it by hand, you could mess it up, so be careful.
 ;; Your init file should contain only one such instance.
 ;; If there is more than one, they won't work right.
 )
