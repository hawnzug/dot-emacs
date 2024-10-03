;;;; Package Management
(setopt use-package-enable-imenu-support t)
(require 'use-package)

(dolist (path (directory-files package-user-dir))
  (when-let (((not (member path '("." ".." "archives" "gnupg"))))
             (abspath (expand-file-name path package-user-dir))
             ((file-directory-p abspath)))
    (add-to-list 'load-path abspath)))

(with-eval-after-load 'info
  (info-initialize)
  (dolist (dir (directory-files package-user-dir))
    (let ((fdir (concat (file-name-as-directory package-user-dir) dir)))
      (unless (or (member dir '("." ".." "archives" "gnupg"))
                  (not (file-directory-p fdir))
                  (not (file-exists-p (concat (file-name-as-directory fdir) "dir"))))
        (add-to-list 'Info-directory-list fdir)))))

(use-package package
  :defer t
  :config
  (setq package-install-upgrade-built-in t)
  (add-to-list 'package-archives
               '("melpa" . "https://melpa.org/packages/") t))

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

(use-package comp
  ;; Already loaded before init
  :config
  (setopt native-comp-async-report-warnings-errors 'silent))

(load "~/.config/emacs/var/personal")

;;;; User Interface
(setq custom-safe-themes t)

(load-theme 'fourma t)

(defun my:font-setup ()
  (let (
        (primary-font "Rec Mono Casual 14")
        (primary-font "DejaVu Sans Mono 14")
        (primary-font "Iosevka Curly Slab 14")
        (primary-font "FreeMono 14")
        (primary-font "JetBrains Mono NL 14")
        (primary-font "Iosevka SS15 Extended 14")
        (primary-font (font-spec :family "Iosevka"
                                 :size 22.0
                                 :weight 'normal))
        (chinese-font (font-spec :family "FZGuoMeiJinDaoTi"))
        (chinese-font (font-spec :family "Source Han Serif CN"
                                 :weight 'bold)))
    (setq face-font-rescale-alist '(("Source Han Serif CN" . 0.825)))
    (set-fontset-font t 'greek primary-font)
    (set-fontset-font t 'greek "JetBrains Mono NL" nil 'append)
    (set-fontset-font t 'greek "DejaVu Sans Mono" nil 'append)
    (set-fontset-font t 'symbol primary-font)
    (set-fontset-font t 'symbol "JetBrains Mono NL" nil 'append)
    (set-fontset-font t 'symbol "DejaVu Sans Mono" nil 'append)
    (set-fontset-font t 'unicode primary-font)
    (set-fontset-font t 'unicode "JetBrains Mono NL" nil 'append)
    (set-fontset-font t 'unicode "Noto Color Emoji" nil 'append)
    (set-fontset-font t 'unicode "Symbols Nerd Font Mono" nil 'append)
    (set-fontset-font t 'unicode "DejaVu Sans" nil 'append)

    (set-fontset-font t 'han primary-font)
    (set-fontset-font t 'han "JetBrains Mono NL" nil 'append)
    (set-fontset-font t 'han chinese-font nil 'append)
    (set-fontset-font t 'cjk-misc primary-font)
    (set-fontset-font t 'cjk-misc "JetBrains Mono NL" nil 'append)
    (set-fontset-font t 'cjk-misc chinese-font nil 'append)
    (set-fontset-font t 'chinese-gbk primary-font)
    (set-fontset-font t 'chinese-gbk "JetBrains Mono NL" nil 'append)
    (set-fontset-font t 'chinese-gbk chinese-font nil 'append)
    (set-face-font 'default primary-font)
    (set-face-font 'fixed-pitch primary-font)
    (set-face-font 'fixed-pitch-serif primary-font)
    (set-face-font 'variable-pitch "Alegreya 22")))

(defun my:font-setup-hook (frame)
  "Setup the font, then remove the hook."
  (select-frame frame)
  (my:font-setup)
  (remove-hook 'after-make-frame-functions 'my:font-setup-hook))
(setq use-default-font-for-symbols nil)
(setq inhibit-compacting-font-caches t)
(setq ring-bell-function 'ignore)
(setopt delete-pair-blink-delay 0)
(setopt line-spacing nil)
(add-hook 'after-make-frame-functions 'my:font-setup-hook nil)
(my:font-setup)

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
  :ensure t
  :init
  (require 'nerd-icons-autoloads))

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
  :defer 1
  :config
  (setopt
   pixel-scroll-precision-use-momentum t
   pixel-scroll-precision-interpolate-page t)
  (pixel-scroll-precision-mode 1))

(use-package indent-bars
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
(use-package tooe-colemak
  :load-path "~/Dev/tooe"
  :config
  (tooe-mode)
  (define-keymap
    :keymap tooe-normal-map
    "," #'avy-goto-char-timer))

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

(define-keymap
  :keymap ctl-x-map
  "f" #'find-file
  "b" #'consult-buffer
  "c" #'org-capture
  "v" #'vterm
  ;; swap C-x C-e and C-x e
  "e" #'eval-last-sexp
  "C-e" #'kmacro-end-and-call-macro)

(keymap-global-set "C-=" #'text-scale-adjust)
(keymap-global-set "C--" #'text-scale-adjust)

(dolist (km (list minibuffer-mode-map
                  minibuffer-local-map
                  minibuffer-local-ns-map
                  minibuffer-local-completion-map
                  minibuffer-local-must-match-map
                  minibuffer-local-isearch-map))
  (keymap-set km "<escape>" 'abort-minibuffers))

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
  (setq corfu-auto t)
  (global-corfu-mode))

(use-package emacs
  :init
  (setq completion-cycle-threshold 3)
  (setq read-extended-command-predicate
        #'command-completion-default-include-p)
  (setq tab-always-indent 'complete))

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
  :commands
  (consult-line
   consult-buffer
   consult-recent-file
   consult-ripgrep))
(use-package consult-xref
  :after (xref consult)
  :config
  (setq xref-show-xrefs-function #'consult-xref
        xref-show-definitions-function #'consult-xref))
(use-package consult-org
  :after org
  :commands consult-org-heading)
(use-package consult-imenu
  :commands consult-imenu)
(use-package consult-flymake
  :after flymake
  :commands consult-flymake)
(use-package consult-register
  :commands
  (consult-register
   consult-register-load
   consult-register-store))
(use-package consult-info
  :commands
  (consult-info))

(defvar-keymap my:consult-map
  "g" #'consult-line
  "i" #'consult-imenu
  "o" #'consult-org-heading
  "r" #'consult-ripgrep
  "l" #'consult-goto-line
  "m" #'consult-mark
  "d" #'consult-fd
  "h" #'consult-recent-file
  "n" #'consult-info
  "f" #'consult-flymake)
(keymap-set tooe-normal-map "g" my:consult-map)

(use-package embark
  :ensure t
  :commands (embark-act embark-dwim embark-bindings embark-prefix-help-command)
  :init
  (setq prefix-help-command #'embark-prefix-help-command)
  (keymap-global-set "C-." #'embark-act)
  (keymap-global-set "M-." #'embark-dwim)
  (keymap-global-set "C-h B" #'embark-bindings))

(use-package embark-consult
  :ensure t
  :after embark)

(use-package avy
  :ensure t
  :commands avy-goto-char-timer
  :config
  (setq avy-timeout-seconds 0.25))

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
    "bsrc" (lambda () (interactive)
               (insert "#+BEGIN_SRC elisp\n#+END_SRC")
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
  (require 'rime-autoloads)
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
  :load-path "~/.config/emacs/packages/flypy-re"
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
  ;; :load-path "~/Projects/org-mode/lisp"
  :defer 4
  :mode ("\\.org\\'" . org-mode)
  :init
  (setq org-modules '())
  :hook
  ;; (org-mode . variable-pitch-mode)
  (org-mode . my:show-trailing-space)
  (org-babel-after-execute . org-redisplay-inline-images)
  :config
  (setq org-special-ctrl-a/e t)
  (setq org-todo-keywords
        '((sequence "TODO(t)" "NEXT(n)" "|" "DONE(d)" "SOMEDAY(s)")))
  (setopt
   org-todo-keyword-faces '(("NEXT" . (:background "#E8E3CB" :weight light))))
  (setq org-agenda-files '("~/org/inbox.org"))
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
  (setq org-startup-indented t)
  (setq org-startup-truncated t)
  (setq org-hide-emphasis-markers t)
  (setq org-footnote-section nil))

(use-package org-refile
  :commands org-refile
  :config
  (setq org-refile-targets
        '((nil . (:level . 1))))
  (setq org-refile-use-outline-path nil))

(use-package org-agenda
  :commands org-agenda
  :config
  (use-package org-habit))

(defun my:select-workout ()
  (interactive)
  (completing-read "Workout" my:workout-list nil t))

(use-package org-capture
  :commands org-capture
  :config
  (setq
   org-capture-templates
   '(("i" "Inbox" entry (file+headline "~/org/inbox.org" "Inbox")
      "* %?\n:PROPERTIES:\n:CREATED:  %U\n:END:"
      :prepend t)
     ("w" "Workout" table-line (file "~/workout.org")
      "| %(my:select-workout) | %? |  | %U |  |")
     ("b" "Bookmark" entry (file+headline "~/org/inbox.org" "Inbox")
      "* %a\n:PROPERTIES:\n:CREATED:  %U\n:END:\n%i"
      :prepend t))))

(use-package org-protocol
  :after org)

(use-package org-id
  :after org
  :config
  (setq org-id-link-to-org-use-id 'create-if-interactive))

(use-package org-make-toc
  :ensure t
  :commands (org-make-toc))

(use-package denote
  :ensure t
  :init
  (require 'denote-autoloads)
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
  :commands eshell)

(use-package eat
  :ensure t
  :defer t
  :init
  (load "eat-autoloads"))

(use-package vterm
  :ensure t
  :commands vterm)

(use-package vterm-toggle
  :ensure t
  :commands vterm-toggle
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
  :commands terminal-here-launch
  :config
  (setq terminal-here-terminal-command
        '("alacritty")))

;;;; Version Control, Backup, Autosave
(use-package magit
  :ensure t
  :defer 5
  :init
  (require 'magit-autoloads)
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
  (require 'citar-autoloads)
  :config
  (setopt
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
  :commands (find-file-in-project)
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
  :commands dired
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
  :disabled
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
  :commands ibuffer
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
   tab-bar-format '(tab-bar-format-tabs
                    tab-bar-format-align-right)
   tab-bar-tab-name-format-function 'my:tab-name-format))

(use-package tabspaces
  :ensure t
  :hook (after-init . tabspaces-mode)
  :commands (tabspaces-switch-or-create-workspace
             tabspaces-open-or-create-project-and-workspace)
  :init
  (setopt tabspaces-keymap-prefix nil)
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
  (setopt split-width-threshold 60))

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
  :disabled
  :load-path "~/Projects/emacs-py/lsp-bridge"
  :init
  (defun my:lsp-bridge-mode-set-keymap ()
    (keymap-local-set "RET" #'newline-and-indent))
  (setopt
   lsp-bridge-python-multi-lsp-server "pyright_ruff"
   lsp-bridge-python-command "emacs-python.sh")
  :hook
  ((LaTeX-mode python-mode tuareg-mode
    agda2-mode haskell-mode typescript-mode js-mode js2-mode
    bibtex-mode sh-mode bash-mode web-mode css-mode
    emacs-lisp-mode dockerfile-mode)
   . lsp-bridge-mode)
  :config
  (add-hook 'lsp-bridge-mode-hook #'my:lsp-bridge-mode-set-keymap))

(use-package eglot
  :ensure t
  :commands eglot
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
  :commands comment-dwim
  :init
  (keymap-global-set "C-/" #'comment-dwim))

(use-package symbol-overlay
  :ensure t
  :commands symbol-overlay-put)

(use-package flymake
  :defer t)

(use-package imenu-list
  :ensure t
  :commands imenu-list)

(use-package ediff
  :commands ediff
  :config
  (setq ediff-split-window-function 'split-window-horizontally)
  (setq ediff-window-setup-function 'ediff-setup-windows-plain))

(use-package htmlize
  :ensure t
  :commands (htmlize htmlize-file htmlize-region htmlize-buffer))

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
  :init
  (load "sly-autoloads")
  :config
  (setq inferior-lisp-program "sbcl"))

(defvar-keymap my:sly-mode-leader-map
  "e" #'sly-eval-last-expression
  "l" #'sly-load-file
  "c" #'sly-compile-defun
  "k" #'sly-compile-and-load-file
  "." #'sly-edit-definition
  "," #'sly-pop-find-definition-stack
  "?" #'sly-edit-uses
  "~" #'sly-mrepl-sync
  "z" #'sly-mrepl
  ":" #'sly-interactive-eval
  "f" #'sly-eval-defun
  "w a" #'sly-who-specializes
  "w b" #'sly-who-binds
  "w c" #'sly-who-calls
  "w RET" #'sly-who-macroexpands
  "w r" #'sly-who-references
  "w s" #'sly-who-sets
  "w w" #'sly-calls-who
  "d a" #'sly-apropos
  "d d" #'sly-describe-symbol
  "d f" #'sly-describe-function
  "d g" #'common-lisp-hyperspec-glossary-term
  "d h" #'sly-documentation-lookup
  "d p" #'sly-apropos-package
  "d z" #'sly-apropos-all
  "d r" #'common-lisp-hyperspec-lookup-reader-macro
  "d t" #'common-lisp-hyperspec-format)

(use-package cooltt
  :mode ("\\.cooltt\\'" . cooltt-mode)
  :load-path "~/Projects/cooltt/emacs")

(use-package zig-mode
  :ensure t
  :mode ("\\.zig\\'" . zig-mode))

(use-package cc-mode
  :mode
  (("\\.c\\'" . c-mode)
   ("\\.h\\'" . c-or-c++-mode))
  :config
  (setq c-basic-offset 4))

(use-package modern-cpp-font-lock
  :ensure t
  :hook (c++-mode . modern-c++-font-lock-mode))

(use-package proof-general
  :ensure t
  :mode ("\\.v\\'" . coq-mode)
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
  :mode ("\\.[Cc][Ss][Vv]\\'" . csv-mode))

(use-package dockerfile-mode
  :ensure t
  :mode "Dockerfile\\'")

(use-package haskell-mode
  :ensure t
  :mode "\\.hs\\'"
  :config
  (require 'haskell)
  (require 'haskell-doc))

(use-package markdown-mode
  :ensure t
  :mode "\\.md\\'")

(use-package sgml-mode
  :mode ("\\.html\\'" . html-mode)
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
  :mode ("\\.ts\\'" . typescript-mode))

(use-package json-mode
  :ensure t
  :mode "\\.json\\'")

(use-package lua-mode
  :ensure t
  :mode "\\.lua\\'")

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
  :mode ("\\.ml[ip]?\\'" . tuareg-mode)
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
  :mode "\\.sml\\'"
  :config
  (setq sml-indent-level 4)
  (setq sml-indent-args 2))

(use-package prolog
  :mode ("\\.pl\\'" . prolog-mode)
  :init
  (setq prolog-system 'swi))

(use-package python
  :mode ("\\.py\\'" . python-mode)
  :config
  (with-eval-after-load 'eglot
    (add-to-list
     'eglot-server-programs
     '(python-mode . ("pdm" "run"
                      "pyright-langserver" "--stdio")))))

(use-package cubicaltt
  :load-path "~/cubicaltt"
  :mode ("\\.ctt$" . cubicaltt-mode))

(use-package flymake-shellcheck
  :ensure t
  :hook (sh-mode . flymake-shellcheck-load))

(use-package tex-site
  :ensure auctex
  :defer t
  :init
  (require 'auctex-autoloads)
  :config
  (add-hook 'LaTeX-mode-hook 'my:show-trailing-space)
  (add-hook 'LaTeX-mode-hook 'TeX-source-correlate-mode)
  (add-hook 'LaTeX-mode-hook 'TeX-PDF-mode)
  (setq font-latex-fontify-sectioning 'color)
  (setq font-latex-fontify-script nil)
  (setq TeX-view-program-selection '((output-pdf "Zathura"))))

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
  :mode "\\.yaml\\'")
(use-package ledger-mode
  :ensure t
  :mode "\\.journal\\'"
  :commands ledger-mode
  :config
  (setq ledger-binary-path "ledger.sh")
  (setq ledger-mode-should-check-version nil)
  (setq ledger-report-links-in-register nil)
  (setq ledger-report-auto-width nil)
  (setq ledger-report-use-native-highlighting nil))

(use-package markdown-mode
  :ensure t
  :commands (gfm-view-mode markdown-view-mode)
  :mode (("README\\.md\\'" . gfm-mode)
         ("\\.md\\'" . markdown-mode)
         ("\\.mkd\\'" . markdown-mode)
         ("\\.markdown\\'" . markdown-mode)))
;;;; Dashboard
(use-package elfeed
  :ensure t
  :commands (elfeed elfeed-update)
  :config
  (setq elfeed-feeds my:elfeed-list))

(use-package elfeed-score
  :ensure t
  :after elfeed
  (elfeed-score-enable)
  (define-key elfeed-search-mode-map "=" elfeed-score-map)
  (setq elfeed-search-print-entry-function #'elfeed-score-print-entry))

(use-package pomidor
  :ensure t
  :commands pomidor
  :init
  (keymap-global-set "<f12>" #'pomidor)
  :config
  (setq alert-default-style 'notifications)
  (setopt pomidor-sound-tick nil)
  (setopt pomidor-sound-tack nil)
  (setopt pomidor-save-session-file
          (no-littering-expand-var-file-name "pomidor-session.json")))

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

;;;; Custom
(setq custom-file "~/.config/emacs/emacs-custom.el")
(load custom-file)
