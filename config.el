;;;; Package Management
(setopt use-package-enable-imenu-support t)
(require 'use-package)

(add-to-list 'load-path "~/.config/emacs/lisp")

;; (dolist (path (directory-files package-user-dir))
;;   (when-let (((not (member path '("." ".." "archives" "gnupg"))))
;;              (abspath (expand-file-name path package-user-dir))
;;              ((file-directory-p abspath)))
;;     (add-to-list 'load-path abspath)))

;; (with-eval-after-load 'info
;;   (info-initialize)
;;   (dolist (dir (directory-files package-user-dir))
;;     (let ((fdir (concat (file-name-as-directory package-user-dir) dir)))
;;       (unless (or (member dir '("." ".." "archives" "gnupg"))
;;                   (not (file-directory-p fdir))
;;                   (not (file-exists-p (concat (file-name-as-directory fdir) "dir"))))
;;         (add-to-list 'Info-directory-list fdir)))))

(use-package package
  :defer t
  :config
  (setq package-install-upgrade-built-in t)
  (add-to-list 'package-archives
               '("melpa" . "https://melpa.org/packages/") t))
(use-package package-vc
  :defer t
  :config
  (setopt package-vc-allow-build-commands t))

(setq inhibit-startup-screen t)
(setq inhibit-startup-echo-area-message t)
(setq initial-scratch-message nil)
(setq initial-major-mode 'fundamental-mode)

;;;; Sane Setup
(use-package no-littering
  :ensure)

(use-package files
  ;; Already loaded before init
  :config
  (setopt
   make-backup-files nil
   auto-save-default nil
   auto-save-visited-interval 1)
  (auto-save-visited-mode))
(use-package emacs
  :config
  (setopt
   word-wrap-by-category t
   delete-by-moving-to-trash t
   text-quoting-style 'straight
   frame-resize-pixelwise t)
  ;; long lines
  (setq-default bidi-display-reordering nil)
  (setq bidi-inhibit-bpa t
        long-line-threshold 1000
        large-hscroll-threshold 1000
        syntax-wholeline-max 1000)
  (setq
   default-process-coding-system '(utf-8-unix . utf-8-unix)
   kill-buffer-query-functions nil
   read-process-output-max (* 1024 1024))
  (defun system-move-file-to-trash (filename)
    (shell-command (concat "trash " (shell-quote-argument filename))))
  (defalias 'yes-or-no-p 'y-or-n-p)
  (setq-default tab-width 4)
  (setq-default truncate-lines t))

(use-package gcmh
  :ensure t
  :config
  (setopt gcmh-idle-delay 10)
  (gcmh-mode 1))

(use-package comp
  ;; Already loaded before init
  :config
  (setopt native-comp-async-report-warnings-errors 'silent))

(load "~/.config/emacs/var/personal")

;;;; User Interface
(defun my:font-setup ()
  (interactive)
  (let ((primary-font (font-spec :family "Iosevka Nono" :size 22.0 :weight 'normal))
        (chinese-font (font-spec :family "Source Han Serif CN" :weight 'bold)))
    (setq inhibit-compacting-font-caches t)
    (setq use-default-font-for-symbols nil)
    (setq xft-ignore-color-fonts nil)
    (setq face-ignored-fonts nil)
    (setq face-font-rescale-alist '(("Source Han Serif CN" . 0.825)))
    (dolist (characters '(greek symbol unicode))
      (set-fontset-font t characters primary-font)
      (set-fontset-font t characters "Noto Color Emoji" nil 'append)
      (set-fontset-font t characters "Symbols Nerd Font Mono" nil 'append))
    (dolist (characters '(han chinese-gbk cjk-misc))
      (set-fontset-font t characters primary-font)
      (set-fontset-font t characters chinese-font nil 'append))
    (set-face-font 'default primary-font)
    (set-face-font 'fixed-pitch primary-font)
    (set-face-font 'fixed-pitch-serif primary-font)
    (set-face-font 'variable-pitch "Alegreya 22")))

;; (add-hook 'after-make-frame-functions #'my:font-setup nil)
(my:font-setup)

(setq ring-bell-function 'ignore)
(setopt delete-pair-blink-delay 0)
(setopt line-spacing nil)

;; (setq custom-safe-themes t)
(load-theme 'fourma :no-confirm)
(use-package modus-themes
  :disabled
  :ensure t
  :config
  (setq modus-themes-italic-constructs t
        modus-themes-bold-constructs t)
  (load-theme 'modus-operandi :no-confirm))

(defun my:syntax-color-hex ()
  (interactive)
  (font-lock-add-keywords
   nil
   '(("#[[:xdigit:]]\\{6\\}"
      (0 (put-text-property
          (match-beginning 0)
          (match-end 0)
          'face (list :background (match-string-no-properties 0)))))))
  (font-lock-flush))

(defun my:toggle-line-number ()
  "Toggle line number between relative and nil."
  (interactive)
  (setq display-line-numbers
        (pcase display-line-numbers
          ('relative nil)
          (_ 'relative))))

(defun my:toggle-transparency ()
  (interactive)
  (let ((old-alpha (frame-parameter nil 'alpha-background)))
    (if (and (numberp old-alpha) (< old-alpha 100))
        (set-frame-parameter nil 'alpha-background 100)
      (set-frame-parameter nil 'alpha-background 90))))
(my:toggle-transparency)

(defun my:show-trailing-space ()
  (setq show-trailing-whitespace t))
(add-hook 'prog-mode-hook #'my:show-trailing-space)

(use-package nerd-icons
  :ensure t)

(use-package hide-mode-line
  :ensure t
  :config
  (setq hide-mode-line-excluded-modes nil)
  (global-hide-mode-line-mode))

(use-package doom-modeline
  :disabled
  :ensure t
  :hook (after-init . doom-modeline-mode)
  :config
  (setq doom-modeline-hud nil)
  (setq doom-modeline-height 18))

(use-package frame
  ;; Already loaded before init
  :config
  (setopt
   window-divider-default-right-width 1
   window-divider-default-bottom-width 1
   window-divider-default-places t)
  (modify-all-frames-parameters
   '((internal-border-width . 0)))
  (blink-cursor-mode)
  (window-divider-mode))

(use-package pixel-scroll
  :config
  (setopt pixel-scroll-precision-use-momentum t)
  (setopt pixel-scroll-precision-interpolate-page t)
  (setopt pixel-scroll-precision-interpolation-total-time 0.2)
  (setopt pixel-scroll-precision-interpolation-factor 3.0)
  (setopt pixel-scroll-precision-large-scroll-height 40.0)
  (pixel-scroll-precision-mode 1))

(use-package indent-bars
  :disabled
  :init
  (defun my:agda2-indent-bars-spacing-override ()
    (setq indent-bars-spacing-override 2))
  :hook
  (prog-mode . indent-bars-mode)
  (agda2-mode . my:agda2-indent-bars-spacing-override)
  :config
  (setq
   indent-bars-pattern "."
   indent-bars-width-frac 0.1
   indent-bars-pad-frac 0
   indent-bars-zigzag nil
   indent-bars-color-by-depth '(:palette ("red" "green" "orange" "cyan") :blend 0.1)
   indent-bars-highlight-current-depth '(:blend 1)
   indent-bars-starting-column 0
   indent-bars-display-on-blank-lines nil))

(use-package olivetti
  :disabled
  :ensure t
  :commands olivetti-mode
  :config
  (setq-default olivetti-body-width 0.7)
  (setq olivetti-minimum-body-width 40))

(use-package perfect-margin
  :disabled
  :ensure t
  :custom
  (perfect-margin-visible-width 80)
  :config
  ;; enable perfect-mode
  (perfect-margin-mode t)
  ;; auto-center minibuffer windows
  ;; (setq perfect-margin-ignore-filters nil)
  ;; auto-center special windows
  ;; (setq perfect-margin-ignore-regexps nil)
  (add-to-list 'perfect-margin-force-regexps "*info")
  ;; add additinal bding on margin area
  (dolist (margin '("<left-margin> " "<right-margin> "))
  (global-set-key (kbd (concat margin "<mouse-1>")) 'ignore)
  (global-set-key (kbd (concat margin "<mouse-3>")) 'ignore)
  (dolist (multiple '("" "double-" "triple-"))
      (global-set-key (kbd (concat margin "<" multiple "wheel-up>")) 'mwheel-scroll)
      (global-set-key (kbd (concat margin "<" multiple "wheel-down>")) 'mwheel-scroll))))

(use-package keyfreq
  :ensure t
  :hook
  (after-init . keyfreq-mode)
  (after-init . keyfreq-autosave-mode))

;;;; Modal Editing
(use-package nothing
  :disabled t
  :load-path "~/Dev/nothing"
  :config
  (define-keymap
    :keymap global-map
    "M-w" #'nothing-save
    "C-w" #'nothing-kill))

(use-package easy-kill
  :disabled t
  :ensure t
  :config
  (global-set-key [remap kill-ring-save] 'easy-kill)
  (global-set-key [remap mark-sexp] 'easy-mark))

(use-package puni
  :disabled t
  :ensure t
  :defer t
  :init
  ;; The autoloads of Puni are set up so you can enable `puni-mode` or
  ;; `puni-global-mode` before `puni` is actually loaded. Only after you press
  ;; any key that calls Puni commands, it's loaded.
  (require 'puni-autoloads)
  (puni-global-mode)
  (add-hook 'term-mode-hook #'puni-disable-puni-mode))

(use-package tooe-colemak
  :load-path "~/Dev/tooe"
  :config
  (tooe-mode))

(use-package boon-colemak
  :disabled
  :ensure boon
  :config
  (boon-mode)
  (define-keymap
    :keymap boon-moves-map
    "h" #'avy-goto-char-timer
    "m" #'my:find-char-backward
    "," #'my:till-char-backward
    "." #'my:till-char-forward
    "/" #'my:find-char-forward
    "M" #'backward-up-list
    "?" #'down-list
    "N" #'boon-beginning-of-expression
    "O" #'boon-end-of-expression)
  (define-keymap
    :keymap boon-command-map
    "p" #'consult-line
    "T" #'join-line
    "d" #'boon-treasure-region
    "D" #'boon-replace-by-character))

(use-package repeat
  :hook
  (after-init . repeat-mode))

(keymap-global-set "C-=" #'text-scale-adjust)
(keymap-global-set "C--" #'text-scale-adjust)

;; (dolist (km (list minibuffer-mode-map
;;                   minibuffer-local-map
;;                   minibuffer-local-ns-map
;;                   minibuffer-local-completion-map
;;                   minibuffer-local-must-match-map
;;                   minibuffer-local-isearch-map))
;;   (keymap-set km "<escape>" 'abort-minibuffers))

;;;; Search and Completion
(use-package vertico
  :ensure t
  :hook (after-init . vertico-mode))

;; No need to autoload. It is almost always needed.
(use-package vertico-directory
  :after vertico
  :config
  (define-keymap :keymap vertico-map
    "RET" #'vertico-directory-enter
    "DEL" #'vertico-directory-delete-char
    "M-DEL"  #'vertico-directory-delete-word)
  (add-hook 'rfn-eshadow-update-overlay-hook
            #'vertico-directory-tidy))

(use-package vertico-multiform
  :after vertico
  :config
  (vertico-multiform-mode)
  (setq vertico-multiform-commands
        '((consult-imenu buffer)
          (consult-outline buffer)
          (consult-buffer unobtrusive)))
  (setq vertico-multiform-categories
        '((consult-grep buffer)
          (buffer unobtrusive))))

(use-package orderless
  :ensure t
  :after vertico
  :init
  ;; (defun my:orderless-in-minibuffer ()
  ;;   (setq-local completion-styles '(orderless)))
  ;; (add-hook 'minibuffer-setup-hook
  ;;           'my:orderless-in-minibuffer)
  (setq completion-styles '(orderless basic))
  (setq completion-category-defaults nil)
  (setq completion-category-overrides
        '((file (styles . (partial-completion))))))

(use-package corfu
  :ensure t
  :config
  (with-eval-after-load 'tooe-colemak
    (defun my:corfu-quit-and-escape ()
      (interactive)
      (call-interactively 'corfu-quit)
      (tooe-set-normal-state))
    (keymap-set tooe-insert-map "<escape>" #'my:corfu-quit-and-escape))
  (defun corfu-move-to-minibuffer ()
    (interactive)
    (pcase completion-in-region--data
      (`(,beg ,end ,table ,pred ,extras)
       (let ((completion-extra-properties extras)
             completion-cycle-threshold completion-cycling)
         (consult-completion-in-region beg end table pred)))))
  (keymap-set corfu-map "M-m" #'corfu-move-to-minibuffer)
  (add-to-list 'corfu-continue-commands #'corfu-move-to-minibuffer)
  (setopt corfu-cycle t)
  (setopt corfu-auto t)
  (setopt corfu-auto-delay 0.1)
  (global-corfu-mode))

(use-package emacs
  :init
  (setq completion-cycle-threshold 3)
  (setq read-extended-command-predicate
        #'command-completion-default-include-p)
  (setopt text-mode-ispell-word-completion nil)
  (setq tab-always-indent 'complete))

(use-package corfu-candidate-overlay
  :disabled
  :ensure t
  :after corfu
  :config
  (corfu-candidate-overlay-mode +1)
  (keymap-global-set "C-<tab>" #'corfu-candidate-overlay-complete-at-point))

(use-package cape
  :ensure t
  :init
  (add-to-list 'completion-at-point-functions #'cape-dabbrev))

(use-package marginalia
  :ensure t
  :after vertico
  :config
  (marginalia-mode))

(use-package consult
  :ensure t
  :defer t
  :init
  (keymap-set ctl-x-map "b" #'consult-buffer))
(use-package consult-xref
  :after (xref consult)
  :config
  (setq xref-show-xrefs-function #'consult-xref
        xref-show-definitions-function #'consult-xref))

(define-keymap
  :keymap goto-map
  "e" #'consult-compile-error
  "f" #'consult-flymake
  "i" #'consult-imenu
  "o" #'consult-outline
  "m" #'consult-mark
  "k" #'consult-global-mark
  "g" #'consult-goto-line
  "M-g" #'consult-goto-line)
(define-keymap
  :keymap search-map
  "d" #'consult-fd
  "r" #'consult-ripgrep
  "l" #'consult-line
  "c" #'consult-locate
  "g" #'consult-grep
  "G" #'consult-git-grep)

(use-package consult-dir
  :ensure t
  :after (consult vertico)
  :config
  (keymap-set ctl-x-map "C-d" #'consult-dir)
  (define-keymap
    :keymap vertico-map
    "C-x C-d" #'consult-dir
    "C-x C-j" #'consult-dir-jump-file))

(use-package embark
  :ensure t
  :defer t
  :init
  (setq prefix-help-command #'embark-prefix-help-command)
  (keymap-global-set "C-." #'embark-act)
  (keymap-global-set "M-." #'embark-dwim)
  (keymap-global-set "C-h B" #'embark-bindings))

(use-package embark-consult
  :ensure t
  :after (embark consult))

(use-package avy
  :ensure t
  :defer t
  :config
  (setq avy-timeout-seconds 0.25)
  (setopt avy-style 'de-bruijn
          avy-keys '(?n ?e ?i ?o ?v ?c ?x ?z))
  (define-keymap
    :keymap global-map
    "M-o" #'avy-goto-char-timer))

(use-package isearch
  :config
  (setq isearch-wrap-pause t
        isearch-lazy-count t
        isearch-repeat-on-direction-change nil))

(use-package tempel
  :ensure t
  :after aas)

(use-package aas
  :ensure t
  :hook (org-mode . aas-activate-for-major-mode)
  :hook (agda2-mode . aas-activate-for-major-mode)
  :hook (html-mode . aas-activate-for-major-mode)
  :config
  (aas-set-snippets 'html-mode
    ";tweet" '(tempel "<tweet date=\"" (format-time-string "%F") "\">" n> "<p>" q "</p>" n "</tweet>"))
  (aas-set-snippets 'org-mode
    "bsb" '(lambda ()
             (interactive)
             (let ((name (my:org-complete-special-block)))
               (if (use-region-p)
                   (let ((beg (use-region-beginning))
                         (end (use-region-end)))
                     (set-mark-command)
                     (goto-char end)
                     (insert "#+end_" name "\n")
                     (goto-char beg)
                     (insert "#+begin_" name "\n"))
                 (unless (bolp) (newline))
                 (insert "#+begin_" name "\n")
                 (save-excursion (insert "\n#+end_" name "\n")))))
    "srce" (lambda () (interactive)
               (insert "#+BEGIN_SRC elisp\n#+END_SRC")
               (org-edit-special))
    "srcl" (lambda () (interactive)
               (insert "#+BEGIN_SRC latex\n#+END_SRC")
               (org-edit-special))
    "\\(" '(tempel "\\(" q "\\)")
    "\\[" '(tempel "\\[" n q n "\\]"))

  (defun my:agda-auto-script-condition ()
    "Condition used for auto-sub/superscript snippets."
    (not (or (bobp) (= (1- (point)) (point-min)) (eq ?\s (char-before))))
    nil)
  (aas-set-snippets 'agda2-mode
    ";+" "⁺"
    "::" "∷"
    "==" "≡"
    "leqrsn"
    '(tempel "let open ≡-Reasoning in begin" n>
             (p (my:agda2-match-eq-reasoning) lr noinsert)
             (car lr) n> "≡⟨ ?" q " ⟩" n> (cdr lr) "∎")
    "eqrsn"
    '(tempel (p (my:agda2-match-eq-reasoning) lr noinsert)
             "begin " n> (car lr) n> "≡⟨ ?" q " ⟩" n> (cdr lr) "∎"))
  (aas-set-snippets 'agda2-mode
    :cond #'my:agda-auto-script-condition
    "'" "′"
    "0" "₀"
    "1" "₁"
    "2" "₂"
    "3" "₃"
    "4" "₄"
    "5" "₅"
    "6" "₆"
    "7" "₇"
    "8" "₈"
    "9" "₉"))

(use-package laas
  :ensure t
  :hook ((LaTeX-mode org-mode). laas-mode)
  :config
  (setq laas-enable-auto-space nil)
  (aas-set-snippets 'laas-mode
    "\\af" '(tempel "\\AgdaFunction{" q "}")
    "\\ad" '(tempel "\\AgdaDatatype{" q "}")
    "\\ac" '(tempel "\\AgdaInductiveConstructor{" q "}")
    ))

(use-package wgrep
  :ensure t
  :defer t)

(use-package bookmark
  :config
  (setq bookmark-fontify nil))

;;;; Input Methods and Localization
(use-package rime
  :ensure t
  :defer t
  :init
  (setq default-input-method 'rime)
  :config
  (define-keymap
    :keymap rime-mode-map
    "C-`" #'rime-send-keybinding)
  (setq rime-show-candidate 'posframe)
  (setq rime-show-preedit 'inline)
  (setq rime-disable-predicates
        '(rime-predicate-evil-mode-p
          rime-predicate-after-alphabet-char-p
          rime-predicate-prog-in-code-p
          rime-predicate-space-after-cc-p)))

(use-package jieba
  :load-path "~/.config/emacs/packages/jieba.el"
  :commands jieba-mode)
(use-package flypy-re
  :config
  ;; orderless
  (with-eval-after-load 'orderless
    (defun completion--regex-pinyin (str)
      (orderless-regexp (flypy-re-build-regexp str)))
    (add-to-list 'orderless-matching-styles 'completion--regex-pinyin))
  ;; avy: overload avy-goto-char-timer
  (with-eval-after-load 'avy
    (defun avy-goto-char-timer (&optional arg)
      "Read one or many consecutive chars and jump to the first one.
The window scope is determined by `avy-all-windows' (ARG negates it)."
      (interactive "P")
      (let ((avy-all-windows (if arg
                                 (not avy-all-windows)
                               avy-all-windows)))
        (avy-with avy-goto-char-timer
                  (setq avy--old-cands (avy--read-candidates #'flypy-re-build-regexp))
                  (avy-process avy--old-cands))))))

;;;; Org Mode and Notes
(use-package org
  :init
  (setq org-modules '())
  :hook
  ;; (org-mode . variable-pitch-mode)
  (org-mode . my:show-trailing-space)
  (org-babel-after-execute . org-redisplay-inline-images)
  :config
  (setopt org-use-property-inheritance t)
  (setq org-special-ctrl-a/e t)
  (setq org-todo-keywords
        '((sequence "TODO(t)" "NEXT(n)" "|" "DONE(d)" "SOMEDAY(s)")))
  (setopt
   org-todo-keyword-faces '(("NEXT" . (:background "#E8E3CB" :weight light))))
  (setq org-agenda-files '("~/org/inbox.org" "~/org/memo.org" "~/org/projects.org"))
  (setq org-archive-location "~/org/archive.org::datetree/")
  (setq org-fontify-done-headline nil)
  (setq org-log-done 'time)
  (setq org-log-into-drawer t)
  (setq org-extend-today-until 2)
  (use-package org-mouse)
  (use-package ol-info)
  (use-package ol-doi)
  (use-package ol-bibtex)
  (use-package ol-gnus)
  (add-to-list 'org-file-apps '(t . "xdg-open %s") t)
  (setq org-reverse-note-order nil)
  (setq org-adapt-indentation nil)
  (setq org-startup-indented nil)
  (setq org-startup-truncated t)
  (setq org-hide-emphasis-markers t)
  (setq org-footnote-section nil)
  (defun my:org-complete-special-block ()
    (interactive)
    (completing-read
     "Special block: "
     '("formula" "theorem" "proof" "placefigure" "MPcode"))))

(use-package org-refile
  :defer t
  :config
  (setq org-refile-targets
        '((nil . (:level . 1))))
  (setq org-refile-use-outline-path nil))

(use-package org-agenda
  :defer t
  :config
  (use-package org-habit))

(use-package ox-context
  :load-path "~/Dev/ox-context")

(defun my:select-workout ()
  (interactive)
  (completing-read "Workout" my:workout-list nil t))

(use-package org-capture
  :defer t
  :init
  (keymap-set ctl-x-map "c" #'org-capture)
  :config
  (setq
   org-capture-templates
   '(("j" "Journal" entry (file+olp+datetree "~/org/inbox.org" "Journal")
      "* %?\n:PROPERTIES:\n:CREATED:  %U\n:END:")
     ("b" "Bookmark" entry (file+olp+datetree "~/org/inbox.org" "Journal")
      "* %a\n:PROPERTIES:\n:CREATED:  %U\n:END:\n%i"))))

(use-package org-protocol
  :after org)

(use-package org-id
  :after org
  :config
  (setq org-id-link-to-org-use-id 'create-if-interactive))

(use-package org-node
  :ensure t
  :after org
  :config
  (org-node-cache-mode))

(use-package org-make-toc
  :ensure t
  :defer t)

(use-package denote
  :ensure t
  :defer t
  :config
  (setopt
   denote-directory (expand-file-name "~/org/notes/")
   denote-infer-keywords t
   denote-sort-keywords t
   denote-date-prompt-use-org-read-date t
   denote-backlinks-show-context t
   denote-dired-directories (list denote-directory))
  (denote-rename-buffer-mode)
  (with-eval-after-load 'dired
    (add-hook 'dired-mode-hook #'denote-dired-mode-in-directories)))

;;;; Shell and Terminal
(use-package eshell
  :defer t
  :init
  (keymap-set ctl-x-map "e" #'eshell)
  :config
  (setopt eshell-visual-commands nil)
  (setopt eshell-history-size 100000)
  (use-package eat
    :config
    (eat-eshell-mode)))

(use-package eat
  :vc (:url "https://codeberg.org/hawnzug/emacs-eat.git"
       :rev :newest
       :doc "eat.texi")
  :defer t)

(use-package vterm
  :ensure t
  :defer t
  :init
  (keymap-set ctl-x-map "v" #'vterm))

(use-package vterm-toggle
  :ensure t
  :defer t
  :config
  (setq vterm-toggle-scope 'project))

(use-package exec-path-from-shell
  :ensure t
  :defer 1
  :config
  (setq exec-path-from-shell-check-startup-files nil)
  (exec-path-from-shell-copy-env "SSH_AGENT_PID")
  (exec-path-from-shell-copy-env "SSH_AUTH_SOCK"))

(use-package terminal-here
  :ensure t
  :defer t
  :config
  (setq terminal-here-terminal-command
        '("alacritty")))

;;;; Version Control, Backup, Autosave
(use-package magit
  :ensure t
  :defer 5
  :config
  (setq magit-repository-directories
        '(("~/.config/emacs" . 0)
          ("~/org" . 0)
          ("~/Dev" . 1)))
  (setq
   magit-repolist-columns
   '(("Name" 15 magit-repolist-column-ident nil)
     ("Flag" 4 magit-repolist-column-flag nil)
     ("B<U" 3 magit-repolist-column-unpulled-from-upstream
      ((:right-align t)
       (:sort <)))
     ("B>U" 3 magit-repolist-column-unpushed-to-upstream
      ((:right-align t)
       (:sort <)))
     ("Branch" 15 magit-repolist-column-branch nil)
     ("Path" 99 magit-repolist-column-path nil))))

(use-package magit-delta
  :ensure t
  :after magit
  :hook (magit-mode . magit-delta-mode)
  :config
  (add-to-list 'magit-delta-delta-args "--max-line-length=2048"))

(use-package vc
  :defer t
  :config
  (with-eval-after-load 'tramp
    (setq vc-ignore-dir-regexp
          (format "\\(%s\\)\\|\\(%s\\)"
                  vc-ignore-dir-regexp
                  tramp-file-name-regexp))))

;;;; Citation Management
(use-package emacs
  :after bibtex
  :config
  (defun my:bibtex-insert-download-by-doi (doi)
    (interactive "sDOI: " bibtex-mode)
    (require 'biblio)
    (let ((biblio-synchronous t)
          (biblio-bibtex-use-autokey t)
          title)
      (insert "\n")
      (biblio-doi-insert-bibtex doi)
      (bibtex-beginning-of-entry)
      (setq title (string-replace "\n" "" (bibtex-text-in-field "title")))
      (bibtex-beginning-first-field)
      (bibtex-make-field
       (list "file" nil (concat title ".pdf") nil))
      (let ((filename (file-name-concat
                       "~/Documents/"
                       (concat title ".pdf"))))
        (pcase (completing-read "PDF: " '("ACM" "SciHub" "SKIP") nil t)
          ("ACM"
           (url-copy-file (concat "https://dl.acm.org/doi/pdf/" doi) filename))
          ("SciHub"
           (require 'scihub)
           (scihub doi filename))
          (_
           (kill-new title)
           (message "Copied to clipboard: %s" title)))))))

(use-package citar
  :ensure t
  :defer t
  :init
  (keymap-set ctl-x-map "l" #'citar-open)
  :config
  (require 'my:citar-notes)
  (setopt
   citar-notes-source 'my:citar-notes
   citar-bibliography '("~/org/refs.bib" "~/org/incomplete.bib")
   citar-library-paths '("~/Documents/")
   citar-file-open-functions (list (cons "html" #'citar-file-open-external)
                                   (cons "pdf" #'citar-file-open-external)
                                   (cons t #'find-file)))
  (with-eval-after-load 'oc
    (setopt
     org-cite-global-bibliography citar-bibliography
     org-cite-insert-processor 'citar
     org-cite-follow-processor 'citar
     org-cite-activate-processor 'citar)))
(use-package citar-capf
  :after citar
  :hook
  (LaTeX-mode . citar-capf-setup)
  (org-mode . citar-capf-setup))
(use-package citar-embark
  :ensure t
  :after citar embark
  :config (citar-embark-mode))

(use-package citar-denote
  :disabled t
  :after (citar denote)
  :ensure t
  :config
  (citar-denote-mode))

(use-package scihub
  :load-path "~/.config/emacs/packages/scihub.el"
  :defer t
  :config
  (setq scihub-download-directory "~/Documents/")
  (setq scihub-open-after-download nil))

;;;; Project and File
(use-package find-file-in-project
  :ensure t
  :defer t
  :config
  (setq ffip-use-rust-fd t))

(use-package project
  :defer t
  :config
  ;; Copy project-shell
  (defun project-vterm ()
    "Start a vterm in the current project's root directory.
If a buffer already exists for running a vterm in the project's root,
switch to it.  Otherwise, create a new vterm buffer.
With \\[universal-argument] prefix arg, create a new vterm buffer even
if one already exists."
    (interactive)
    (require 'vterm)
    (let* ((default-directory (project-root (project-current t)))
           (default-project-vterm-name (project-prefixed-buffer-name "vterm"))
           (vterm-buffer (get-buffer default-project-vterm-name)))
      (if (and vterm-buffer (not current-prefix-arg))
          (pop-to-buffer vterm-buffer)
        (vterm default-project-vterm-name))))
  (define-keymap
    :keymap project-prefix-map
    "v" #'project-vterm))

(use-package dired
  :hook (dired-mode . dired-hide-details-mode)
  :config
  (setq dired-dwim-target t)
  (setq dired-recursive-copies 'always)
  (setq dired-recursive-deletes 'always)
  (setq dired-listing-switches
        "-l --almost-all --human-readable --group-directories-first --no-group")
  (setq dired-isearch-filenames 'dwim))

(use-package dired-x
  :hook (dired-mode . dired-omit-mode)
  :config
  (add-to-list 'dired-omit-extensions ".agdai"))

(use-package dirvish
  :ensure t
  :defer t
  :init
  (require 'dirvish-autoloads)
  (dirvish-override-dired-mode)
  :config
  (define-keymap
    :keymap dirvish-mode-map
    "a" #'dirvish-quick-access
    "f" #'dirvish-file-info-menu
    "y" #'dirvish-yank-menu
    "N" #'dirvish-narrow
    "^" #'dirvish-history-last
    "h" #'dirvish-history-jump
    "s" #'dirvish-quicksort
    "v" #'dirvish-vc-menu
    "TAB" #'dirvish-subtree-toggle
    "M-f" #'dirvish-history-go-forward
    "M-b" #'dirvish-history-go-backward
    "M-l" #'dirvish-ls-switches-menu
    "M-m" #'dirvish-mark-menu
    "M-t" #'dirvish-layout-toggle
    "M-s" #'dirvish-layout-switch
    "M-e" #'dirvish-emerge-menu
    "M-j" #'dirvish-fd-jump)
  (setopt dirvish-use-mode-line 'global)
  (setopt dirvish-use-header-line 'global)
  (setopt dirvish-attributes
          '(nerd-icons file-time file-size collapse subtree-state vc-state git-msg))
  (setopt dirvish-subtree-state-style 'nerd)
  (setopt
   dirvish-path-separators
   (list
    (format "  %s " (nerd-icons-codicon "nf-cod-home"))
    (format "  %s " (nerd-icons-codicon "nf-cod-root_folder"))
    (format " %s " (nerd-icons-faicon "nf-fa-angle_right")))))

(use-package tramp
  :defer t)

(use-package tramp-container
  :after tramp)

(use-package recentf
  :hook (after-init . recentf-mode)
  :config
  (setq recentf-max-saved-items 10000))

;;;; Buffer and Window
(use-package ibuffer
  :defer t
  :config
  (add-hook 'ibuffer-mode-hook #'ibuffer-vc-set-filter-groups-by-vc-root)
  (setq
   ibuffer-formats
   '(("    " (name 24 24) " " (mode 24 24) " " filename-and-process)))
  (use-package ibuffer-vc :ensure t))


(defun my:tab-name-format (tab i)
  (let ((face (funcall tab-bar-tab-face-function tab)))
    (propertize (concat " " (number-to-string i) " "
                        (alist-get 'name tab) " ")
                'face face)))
(use-package tab-bar
  :hook (window-setup . tab-bar-mode)
  :config
  (setq tab-bar-separator "")
  (setopt
   tab-bar-select-tab-modifiers '(meta)
   tab-bar-tab-name-truncated-max 20
   tab-bar-auto-width nil
   tab-bar-new-tab-to 'rightmost
   tab-bar-show 1
   tab-bar-close-button-show nil
   tab-bar-new-tab-choice "*scratch*"
   tab-bar-tab-hints t
   tab-bar-format '(tab-bar-format-tabs-groups
                    tab-bar-format-align-right)
   tab-bar-tab-name-format-function 'my:tab-name-format))

(use-package tabspaces
  :disabled
  :ensure t
  :hook (after-init . tabspaces-mode)
  :init
  (setq tabspaces-keymap-prefix nil)
  :config
  (define-keymap
    :keymap ctl-x-map
    "w" tabspaces-command-map)
  (setopt
   tabspaces-use-filtered-buffers-as-default t
   tabspaces-default-tab "org"
   tabspaces-remove-to-default t
   tabspaces-include-buffers '("*scratch*")
   tabspaces-initialize-project-with-todo nil
   tabspaces-session t
   tabspaces-session-auto-restore t)
  (with-eval-after-load 'consult
    (consult-customize consult--source-buffer :hidden t :default nil)
    (defvar consult--source-workspace
      (list :name     "Workspace Buffers"
            :narrow   ?w
            :history  'buffer-name-history
            :category 'buffer
            :state    #'consult--buffer-state
            :default  t
            :items    (lambda () (consult--buffer-query
                                  :predicate #'tabspaces--local-buffer-p
                                  :sort 'visibility
                                  :as #'buffer-name)))
      "Set workspace buffer list for consult-buffer.")
    (add-to-list 'consult-buffer-sources 'consult--source-workspace)))

(use-package window
  ;; Already loaded before init
  :config
  (setopt split-width-threshold 140))

(use-package help
  ;; Already loaded before init
  :config
  (setopt help-window-select t))

(use-package winner
  :hook
  (after-init . winner-mode)
  (ediff-quit . winner-undo))

;;;; Programming Utilities
(use-package simple
  ;; Already loaded before init
  :config
  (setopt indent-tabs-mode nil))

(use-package elec-pair
  :hook (after-init . electric-pair-mode))

;; Use lsp-bridge for lsp primarily.
;; If something doesn't work, try eglot instead.

(use-package lsp-bridge
  :load-path "~/Projects/emacs-py/lsp-bridge"
  :init
  (setopt
   lsp-bridge-python-multi-lsp-server "pyright_ruff"
   lsp-bridge-python-command "emacs-python.sh")
  :hook
  ((LaTeX-mode python-mode tuareg-mode
    agda2-mode haskell-mode typescript-mode js-mode js2-mode
    bibtex-mode sh-mode bash-mode web-mode css-mode
    emacs-lisp-mode dockerfile-mode)
   . lsp-bridge-mode))

(use-package eglot
  :ensure t
  :defer t
  :init
  (add-hook
   'eglot-managed-mode-hook
   (lambda ()
     ;; Show flymake diagnostics first.
     (setq eldoc-documentation-functions
           (cons #'flymake-eldoc-function
                 (remove #'flymake-eldoc-function eldoc-documentation-functions)))
     ;; Show all eldoc feedback.
     (setq eldoc-documentation-strategy #'eldoc-documentation-compose))))

(use-package consult-eglot
  :ensure t
  :after (consult eglot))

(use-package eldoc
  :defer t
  :config
  (setq eldoc-idle-delay 0)
  (setq eldoc-echo-area-use-multiline-p t)
  (setq eldoc-echo-area-display-truncation-message nil)
  (setq eldoc-echo-area-prefer-doc-buffer t))

(use-package eldoc-box
  :ensure t
  :hook
  (eldoc-mode . eldoc-box-hover-at-point-mode)
  :config
  (setq eldoc-box-max-pixel-width 3000)
  (setq eldoc-box-max-pixel-height 2000))

(use-package hl-todo
  :ensure t
  :hook (prog-mode . hl-todo-mode))

(use-package newcomment
  :defer t
  :init
  (keymap-global-set "C-/" #'comment-dwim))

(use-package symbol-overlay
  :ensure t
  :defer t)

(use-package flymake
  :defer t)

(use-package imenu-list
  :ensure t
  :defer t)

(use-package ediff
  :defer t
  :config
  (setq ediff-split-window-function 'split-window-horizontally)
  (setq ediff-window-setup-function 'ediff-setup-windows-plain))

(use-package htmlize
  :ensure t
  :defer t)

(use-package outline
  :hook ((LaTeX-mode prog-mode) . outline-minor-mode)
  :config
  (setq outline-navigation-repeat-map
        (define-keymap
          "l" #'outline-backward-same-level
          "u" #'outline-forward-same-level
          "n" #'outline-next-visible-heading
          "e" #'outline-previous-visible-heading
          "h" #'outline-up-heading))
  (setq outline-editing-repeat-map
        (define-keymap
          "n" #'outline-move-subtree-down
          "e" #'outline-move-subtree-up
          "i" #'outline-demote
          "h" #'outline-promote))
  (defvar-keymap my:outline-prefix-map
    :parent outline-navigation-repeat-map
    "m" outline-editing-repeat-map)
  (setq outline-minor-mode-cycle t))

(use-package hideshow
  :hook (LaTeX-mode . hs-minor-mode)
  :config
  ;; From Doom Emacs
  (add-to-list 'hs-special-modes-alist
               '(latex-mode
                 ;; LaTeX-find-matching-end needs to be inside the env
                 ("\\\\begin{[a-zA-Z*]+}\\(\\)" 1)
                 "\\\\end{[a-zA-Z*]+}"
                 "%"
                 (lambda (_arg)
                   ;; Don't fold whole document, that's useless
                   (unless (save-excursion
                             (search-backward "\\begin{document}"
                                              (line-beginning-position) t))
                     (LaTeX-find-matching-end))))))

;;;; Programming Languages
(use-package text-mode
  :defer t
  :config
  (setopt text-mode-ispell-word-completion nil))

(use-package sly
  :ensure t
  :defer t
  :config
  (setq inferior-lisp-program "sbcl")
  (setq sly-default-lisp 'sbcl)
  (setq sly-lisp-implementations
        '((sbcl  ("vend" "repl" "sbcl")  :coding-system utf-8-unix)
          (ecl   ("vend" "repl" "ecl")   :coding-system utf-8-unix)
          (abcl  ("vend" "repl" "abcl")  :coding-system utf-8-unix)
          (clasp ("vend" "repl" "clasp") :coding-system utf-8-unix))))

(use-package cooltt
  :load-path "~/Projects/cooltt/emacs"
  :defer t)

(use-package zig-mode
  :ensure t
  :defer t)

(use-package cc-mode
  :defer t
  :config
  (setq c-basic-offset 4))

(use-package modern-cpp-font-lock
  :ensure t
  :defer t)

(use-package proof-general
  :ensure t
  :defer t
  :config
  (setq proof-splash-enable nil))
(use-package company-coq
  :ensure t
  :after proof-site
  :hook (coq-mode . company-coq-mode)
  :config
  (setq company-coq-disabled-features '(smart-subscripts))
  (company-coq--init-refman-ltac-abbrevs-cache)
  (company-coq--init-refman-scope-abbrevs-cache)
  (company-coq--init-refman-tactic-abbrevs-cache)
  (company-coq--init-refman-vernac-abbrevs-cache)
  (defun my:company-coq-doc-search ()
    "Search identifier in coq refman"
    (interactive)
    (ivy-read
     "doc: "
     (append company-coq--refman-tactic-abbrevs-cache
             company-coq--refman-vernac-abbrevs-cache
             company-coq--refman-scope-abbrevs-cache
             company-coq--refman-ltac-abbrevs-cache)
     :preselect (ivy-thing-at-point)
     :action 'company-coq-doc-buffer-refman)))

(use-package csv-mode
  :ensure t
  :defer t)

(use-package dockerfile-mode
  :ensure t
  :defer t)

(use-package haskell-mode
  :ensure t
  :defer t
  :config
  (require 'haskell)
  (require 'haskell-doc))

(use-package markdown-mode
  :ensure t
  :defer t)

(use-package sgml-mode
  :defer t
  :config
  (defun my:html-mode-face-remap ()
    (face-remap-set-base
     'tree-sitter-hl-face:attribute
     :foreground "#AAAAAA")
    (face-remap-set-base
     'tree-sitter-hl-face:punctuation.bracket
     :foreground "#DDDDDD")
    (face-remap-add-relative
     'tree-sitter-hl-face:tag
     :foreground "#AAAAAA"))
  (add-hook 'html-mode-hook 'my:html-mode-face-remap))

(use-package emmet-mode
  :ensure t
  :hook (sgml-mode css-mode))

(use-package typescript-mode
  :ensure t
  :defer t)

(use-package json-mode
  :ensure t
  :defer t)

(use-package lua-mode
  :ensure t
  :defer t)

(eval-and-compile
  (defun agda-mode-load-path ()
    (file-name-directory (shell-command-to-string "agda-mode locate"))))
(use-package agda2-mode
  :load-path (lambda () (agda-mode-load-path))
  :mode ("\\.l?agda\\'" . agda2-mode)
  :config
  (defun my:agda2-match-eq-reasoning ()
    (with-current-buffer agda2-info-buffer
      (let ((goal-regexp (rx "Goal: " (group (*? anything))
                             "≡" (group (*? anything)) "———"))
            (space-regexp (rx (+ (or space "\n")))))
        (goto-char (point-min))
        (when (re-search-forward goal-regexp)
          (let ((lhs (substring-no-properties (match-string 1)))
                (rhs (substring-no-properties (match-string 2))))
            (cons (replace-regexp-in-string space-regexp " " lhs)
                  (replace-regexp-in-string space-regexp " " rhs)))))))
  (setq outline-regexp "-- #+")
  (define-keymap
    :keymap agda2-mode-map
    "C-c C-," nil
    "C-c ," #'agda2-goal-and-context
    "C-c C-." nil
    "C-c ." #'agda2-goal-and-context-and-inferred
    "C-c C-;" nil
    "C-c ;" #'agda2-goal-and-context-and-checked
    "C-c C-=" nil
    "C-c =" #'agda2-show-constraints
    "C-c C-?" nil
    "C-c C-q" #'agda2-show-goals))

(use-package tuareg
  :ensure t
  :defer t)

(eval-and-compile
  (defun opam-emacs-load-path ()
    (expand-file-name
     "emacs/site-lisp"
     (car (process-lines "opam" "var" "share")))))

(use-package utop
  :load-path (lambda () (opam-emacs-load-path))
  :hook
  (tuareg-mode . utop-minor-mode))

(use-package sml-mode
  :ensure t
  :defer t
  :config
  (setq sml-indent-level 4)
  (setq sml-indent-args 2))

(use-package prolog
  :defer t
  :init
  (setq prolog-system 'swi))

(use-package python
  :defer t
  :config
  (with-eval-after-load 'eglot
    (add-to-list
     'eglot-server-programs
     '((python-mode python-ts-mode)
       "uv" "run" "basedpyright-langserver" "--stdio"))))

(use-package cubicaltt
  :load-path "~/cubicaltt"
  :mode ("\\.ctt$" . cubicaltt-mode))

(use-package flymake-shellcheck
  :ensure t
  :hook (sh-mode . flymake-shellcheck-load))

(use-package tex-site
  :ensure auctex
  :defer t
  :config
  (add-hook 'LaTeX-mode-hook 'my:show-trailing-space)
  (add-hook 'LaTeX-mode-hook 'TeX-source-correlate-mode)
  (add-hook 'LaTeX-mode-hook 'TeX-PDF-mode)
  (setopt ConTeXt-Mark-version "IV")
  (setq font-latex-fontify-sectioning 'color)
  (setq font-latex-fontify-script nil)
  (setq TeX-view-program-selection '((output-pdf "Sioyek"))))

(use-package auctex-latexmk
  :disabled
  :ensure t
  :after tex-site
  :config
  (auctex-latexmk-setup))

(defun my:cdlatex-smarter-tab ()
  "Assuming outline-minor-mode and hs-minor-mode are enabled"
  (cond
   ;; Somehow outline-minor-mode-cycle has no effect.
   ;; Maybe cdlatex overrides the TAB
   ((outline-on-heading-p) (outline-cycle))
   ((save-excursion
      (back-to-indentation)
      (hs-looking-at-block-start-p))
    (save-excursion
      (end-of-line)
      (hs-toggle-hiding)
      t))
   ((save-excursion
      (back-to-indentation)
      (looking-at hs-block-end-regexp))
    (save-excursion
      (back-to-indentation)
      (hs-toggle-hiding)
      t))
   ((or (bolp) (looking-back "^[ \t]+"))
    (LaTeX-indent-line))))

(use-package cdlatex
  :ensure t
  :hook (LaTeX-mode . cdlatex-mode)
  :init
  (setq cdlatex-takeover-parenthesis nil)
  :config
  (add-hook 'cdlatex-tab-hook
            #'my:cdlatex-smarter-tab))

(use-package xenops
  :disabled
  ;; :hook (LaTeX-mode . xenops-mode)
  :ensure t
  :config
  (add-to-list
   'xenops-math-latex-process-alist
   '(xdvsvgm
     :programs ("xelatex" "dvisvgm")
     :description "xdv > svg"
     :message "you need to install the programs: xelatex and dvisvgm."
     :image-input-type "xdv"
     :image-output-type "svg"
     :image-size-adjust (1.7 . 1.5)
     :latex-compiler ("xelatex -interaction nonstopmode -shell-escape -no-pdf -output-directory %o %f")
     :image-converter ("dvisvgm %f -n -b min -c %S -o %O")))
  (setq xenops-math-latex-process 'xdvsvgm)
  (setq xenops-reveal-on-entry t))

(use-package yaml-mode
  :ensure t
  :defer t)
(use-package ledger-mode
  :ensure t
  :defer t
  :config
  (setq ledger-binary-path "ledger.sh")
  (setq ledger-mode-should-check-version nil)
  (setq ledger-report-links-in-register nil)
  (setq ledger-report-auto-width nil)
  (setq ledger-report-use-native-highlighting nil))

(use-package markdown-mode
  :ensure t
  :defer t)

;;;; Dashboard
(use-package server
  :config
  (server-mode))

(use-package kdeconnect
  :ensure t
  :defer t
  :init
  (with-eval-after-load 'embark
    (keymap-set embark-file-map "k" 'kdeconnect-send-file))
  :config
  (setopt kdeconnect-devices my:kdeconnect-devices)
  (setopt kdeconnect-active-device (car kdeconnect-devices)))

(use-package elfeed
  :disabled
  :ensure t
  :defer t
  :config
  (setq elfeed-feeds my:elfeed-list))

(use-package elfeed-score
  :disabled
  :ensure t
  :after elfeed
  :config
  (elfeed-score-enable)
  (define-key elfeed-search-mode-map "=" elfeed-score-map)
  (setq elfeed-search-print-entry-function #'elfeed-score-print-entry))

(use-package pomidor
  :ensure t
  :defer t
  :init
  (keymap-global-set "<f12>" #'pomidor)
  :config
  (setq alert-default-style 'notifications)
  (setopt pomidor-sound-tick nil)
  (setopt pomidor-sound-tack nil)
  (setopt pomidor-save-session-file
          (no-littering-expand-var-file-name "pomidor-session.json")))

(use-package erc
  :defer t
  :config
  (setopt erc-modules (add-to-list 'erc-modules 'sasl)))

(use-package notmuch
  :ensure t)

(use-package sendmail
  :config
  (setopt send-mail-function 'sendmail-send-it)
  (setopt sendmail-program "/usr/bin/msmtp")
  (setopt mail-specify-envelope-from t)
  (setopt mail-envelope-from 'header))
(use-package message
  :config
  (setopt message-sendmail-envelope-from 'header))

(defun my:dashboard ()
  (interactive)
  (tab-bar-switch-to-tab "org")
  (delete-other-windows)
  (find-file "~/org/inbox.org")
  (call-interactively #'org-show-todo-tree)
  (org-remove-occur-highlights)
  (split-window-right)
  (magit-list-repositories))
;; (run-with-idle-timer 120 t #'my:dashboard)

(use-package ultra-scroll
  :vc (:url "https://github.com/jdtsmith/ultra-scroll"
       :rev :newest)
  :init
  (setq scroll-conservatively 101 ; important!
        scroll-margin 0)
  ;; :config
  ;; (defalias 'pixel-scroll-precision-scroll-down-page 'ultra-scroll-down)
  ;; (defalias 'pixel-scroll-precision-scroll-up-page 'ultra-scroll-up)
  )

(use-package doc-view
  :config
  (setopt doc-view-continuous t)
  (setopt doc-view-resolution 300))

(use-package citre
  :ensure t)

(use-package eaf
  :load-path "~/Projects/emacs-py/emacs-application-framework"
  :config
  (setopt eaf-python-command "emacs-python.sh")
  (setq eaf-epc-accept-process-timeout 20))

(use-package eaf-pdf-viewer
  :load-path "~/Projects/emacs-py/eaf-pdf-viewer")

;;;; Custom
(setq custom-file "~/.config/emacs/emacs-custom.el")
(load custom-file)
