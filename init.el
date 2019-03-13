(setq gc-cons-threshold 64000000)
(add-hook 'after-init-hook #'(lambda () (setq gc-cons-threshold 800000)))
(scroll-bar-mode -1)
(horizontal-scroll-bar-mode -1)
(tool-bar-mode -1)
(tooltip-mode -1)
(menu-bar-mode -1)
(show-paren-mode 1)
(electric-pair-mode 1)
(setq inhibit-startup-screen t)
(setq initial-scratch-message "")
(setq inhibit-startup-echo-area-message "zhu")
(setq make-backup-files nil)
(setq auto-save-default nil)
(setq-default indent-tabs-mode nil)
(setq-default tab-width 4)
(setq-default truncate-lines t)
(setq-default fill-column 120)
(defalias 'yes-or-no-p 'y-or-n-p)
(setq window-divider-default-places t)
(setq window-divider-default-bottom-width 1)
(setq window-divider-default-right-width 1)
(window-divider-mode)
(setq-default mode-line-format nil)
(load-theme 'lowlight t)

(require 'package)
(setq package-enable-at-startup nil)
(setq package-archives '(("gnu"   . "http://elpa.emacs-china.org/gnu/")
                         ("melpa" . "http://elpa.emacs-china.org/melpa/")))
(package-initialize)
(unless (package-installed-p 'use-package)
  (package-refresh-contents)
  (package-install 'use-package))
(eval-when-compile (require 'use-package))

(use-package expand-region
  :ensure t
  :defer t
  :config
  (setq expand-region-fast-keys-enabled nil))

(use-package highlight-indent-guides
  :ensure t
  :hook ((prog-mode coq-mode) . highlight-indent-guides-mode)
  :config
  (setq highlight-indent-guides-method 'character)
  (setq highlight-indent-guides-auto-character-face-perc 30)
  (setq highlight-indent-guides-auto-top-character-face-perc 70)
  (setq highlight-indent-guides-responsive 'top)
  (setq highlight-indent-guides-character ?\▏))

(use-package company :ensure t :defer t)

(use-package proof-general
  :ensure t
  :config
  (setq proof-splash-enable nil))

(use-package company-coq
  :ensure t
  :after proof-site
  :hook (coq-mode . company-coq-mode)
  :config
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

(use-package ediff
  :defer
  :config
  (setq ediff-split-window-function 'split-window-horizontally)
  (setq ediff-window-setup-function 'ediff-setup-windows-plain))

(use-package eyebrowse
  :ensure t
  :config
  (eyebrowse-mode t))

(eval-and-compile
  (defun agda-mode-load-path ()
    (file-name-directory (shell-command-to-string "agda-mode locate"))))
(use-package agda2 :load-path (lambda () (agda-mode-load-path)))

(use-package haskell-mode :ensure t :defer t)
(use-package ghcid :load-path "~/.emacs.d/packages/ghcid")

(use-package magit
  :ensure t
  :defer t)

(use-package calfw
  :ensure t
  :commands cfw:open-calendar-buffer
  :config
  (setq cfw:display-calendar-holidays nil)
  (setq calendar-week-start-day 1))
(use-package calfw-org
  :ensure t
  :commands cfw:open-org-calendar)

(use-package evil
  :ensure t
  :init
  (setq evil-want-abbrev-expand-on-insert-exit nil)
  :config
  (evil-mode 1)
  (setq evil-disable-insert-state-bindings t))
(use-package evil-surround
  :ensure t
  :after evil
  :config
  (global-evil-surround-mode 1))
(use-package evil-matchit
  :ensure t
  :after evil
  :config
  (global-evil-matchit-mode 1))

(use-package rainbow-delimiters
  :ensure t
  :hook ((prog-mode coq-mode) . rainbow-delimiters-mode))

(use-package which-key
  :ensure t
  :config
  (which-key-mode 1))

(use-package hydra
  :ensure t
  :config
  (defhydra hydra-window
    (:color red :hint nil)
    "window"
    ("j" evil-window-down "down" :column "Move")
    ("k" evil-window-up "up")
    ("h" evil-window-left "left")
    ("l" evil-window-right "right")
    ("n" evil-window-next "next" :color blue)
    ("H" evil-window-move-far-left "left" :column "Swap")
    ("L" evil-window-move-far-right "right")
    ("J" evil-window-move-very-bottom "bottom")
    ("K" evil-window-move-very-top "top")
    ("+" evil-window-increase-height "+ h" :column "Size")
    ("-" evil-window-decrease-height "- h")
    (">" evil-window-increase-width "+ w")
    ("<" evil-window-decrease-width "- w")
    ("=" evil-balance-window "balance")
    ("d" evil-window-delete "delete" :color blue :column "Operate")
    ("s" evil-window-split "split")
    ("v" evil-window-vsplit "vsplit")
    ("o" delete-other-windows "only" :color blue)
    ("q" nil "cancel" :color blue))
  (defhydra hydra-buffer
    (:color red :hint nil)
    "buffer"
    ("j" evil-next-buffer "next")
    ("k" evil-prev-buffer "prev")
    ("d" evil-delete-buffer "delete" :color blue)
    ("b" ivy-switch-buffer "switch" :color blue)
    ("q" nil "cancel" :color blue))
  (defhydra hydra-expand-region
    (:color red :hint nil)
    "expand region"
    ("k" er/expand-region "expand")
    ("j" er/contract-region "contract")
    ("c" er/mark-comment "comment")
    ("f" er/mark-defun "defun")
    ("q" nil "quit" :color blue))
  (defhydra hydra-eyebrowse
    (:color blue :hint nil)
    "eyebrowse"
    ("l" eyebrowse-last-window-config "last" :column "Switch")
    ("j" eyebrowse-next-window-config "next" :color red)
    ("k" eyebrowse-prev-window-config "prev" :color red)
    ("s" eyebrowse-switch-to-window-config "switch")
    ("0" eyebrowse-switch-to-window-config-0 "0" :column "Quick")
    ("1" eyebrowse-switch-to-window-config-1 "1")
    ("2" eyebrowse-switch-to-window-config-2 "2")
    ("3" eyebrowse-switch-to-window-config-3 "3")
    ("4" eyebrowse-switch-to-window-config-4 "4")
    ("5" eyebrowse-switch-to-window-config-5 "5")
    ("6" eyebrowse-switch-to-window-config-6 "6")
    ("7" eyebrowse-switch-to-window-config-7 "7")
    ("8" eyebrowse-switch-to-window-config-8 "8")
    ("9" eyebrowse-switch-to-window-config-9 "9")
    ("d" eyebrowse-close-window-config "delete" :column "Modify")
    ("c" eyebrowse-create-window-config "last")
    ("r" eyebrowse-rename-window-config "rename"))
  (defhydra hydra-org-clock
    (:color blue :hint nil)
    "org clock"
    ("g" org-clock-goto "goto")
    ("i" org-clock-in "in")
    ("I" org-clock-in-last "in last")
    ("o" org-clock-out "out")
    ("c" org-clock-cancel "cancel")
    ("r" org-clock-report "report")))

(use-package counsel
  :ensure t
  :config
  (ivy-mode 1)
  (use-package ivy-hydra :ensure t)
  (setq ivy-use-virtual-buffers t)
  (setq ivy-count-format "(%d/%d) ")
  (setq ivy-re-builders-alist '((t . ivy--regex-plus))))
(use-package all-the-icons-ivy
  :ensure t
  :after (all-the-icons counsel)
  :config
  (all-the-icons-ivy-setup))

(use-package cc-mode
  :commands c-mode
  :config
  (setq c-basic-offset 4)
  (setq c-default-style "linux"))

(use-package flycheck
  :ensure t
  :hook (c-mode . flycheck-mode))
(use-package lsp-mode :ensure t :commands lsp)
(use-package lsp-ui :ensure t :commands lsp-ui-mode)
(use-package company-lsp :ensure t :commands company-lsp)
(use-package ccls
  :ensure t
  :defer t
  :hook ((c-mode c++-mode objc-mode) . (lambda () (require 'ccls) (lsp))))

(use-package lua-mode
  :ensure t
  :mode "\\.lua$"
  :interpreter "lua"
  :config
  (setq lua-indent-level 4))

(use-package projectile
  :ensure t
  :defer t
  :config
  (projectile-mode 1)
  (setq projectile-completion-system 'ivy))

(defun my:org-agenda-time-grid-spacing ()
  "Set different line spacing w.r.t. time duration."
  (save-excursion
    (let ((colors (list "#F6B1C3" "#FFFF9D" "#BEEB9F" "#ADD5F7"))
          (pos (point-min))
          (block-minutes 30)
          duration)
      (nconc colors colors)
      (while (setq pos (next-single-property-change pos 'org-hd-marker))
        (goto-char pos)
        (when (and (not (equal pos (point-at-eol)))
                   (setq duration
                         (or (org-get-at-bol 'duration)
                             (when (equal (org-get-at-bol 'org-hd-marker) org-clock-hd-marker)
                               (/ (- (float-time) (float-time org-clock-start-time)) 60)))))
          (let ((line-height (if (< duration block-minutes) 1.0
                               (+ 0.5 (/ duration (* 2.0 block-minutes)))))
                (ov (make-overlay (point-at-bol) (1+ (point-at-eol)))))
            (overlay-put ov 'face `(:background ,(car colors)))
            (setq colors (cdr colors))
            (overlay-put ov 'line-height line-height)
            (overlay-put ov 'line-spacing (1- line-height))))))))

(use-package org
  :init
  (setq org-agenda-files '("~/org/sjtu.org" "~/org/diary.org"))
  (setq org-archive-location "~/org/diary.org::datetree/")
  :hook
  ((org-babel-after-execute . org-redisplay-inline-images)
   (org-agenda-finalize . my:org-agenda-time-grid-spacing))
  :config
  (defun kill-org-src-buffers (&rest args)
    "Kill temporary buffers created by org-src-font-lock-fontify-block."
    (dolist (b (buffer-list))
      (let ((bufname (buffer-name b)))
        (if (string-match-p (regexp-quote "org-src-fontification") bufname)
            (kill-buffer b)))))
  (advice-add 'org-src-font-lock-fontify-block :after #'kill-org-src-buffers)
  (setq org-capture-templates
        '(("t" "Todo" entry (file+headline "~/org/sjtu.org" "Inbox")
           "* TODO %?\n  SCHEDULED: %^T")
          ("w" "Water" entry (file+olp+datetree "~/org/diary.org")
           "* water\n     :PROPERTIES:\n     :volume:   %^{PROMPT}\n     :END:"
           :immediate-finish t)
          ("c" "Clock" entry (file+olp+datetree "~/org/diary.org")
           "* %^{PROMPT}\n"
           :immediate-finish t
           :clock-in t
           :clock-keep t)))
  (setq org-agenda-start-with-log-mode t)
  (setq org-agenda-span 'day)
  (setq org-agenda-log-mode-items '(clock))
  (setq org-agenda-use-time-grid nil)
  (setcdr (assoc "\\.pdf\\'" org-file-apps) "zathura %s")
  (org-clock-persistence-insinuate)
  (setq org-clock-persist t)
  (setq org-clock-in-resume t)
  (setq org-clock-persist-query-resume nil)
  (setq org-confirm-babel-evaluate nil)
  (setq org-clock-clocktable-default-properties
        '(:maxlevel 4 :block today :scope file :link t))
  (setq org-format-latex-options (plist-put org-format-latex-options :scale 2.0))
  (setq org-log-into-drawer t)
  (setq org-log-done 'time)
  (setq org-enforce-todo-dependencies t)
  (setq org-enforce-todo-checkbox-dependencies t)
  (setq org-footnote-section nil))

(defun my:org-refile-to-diary ()
  "Refile a subtree to a datetree corresponding to it's CLOSED time."
  (interactive)
  (let* ((diary-file "~/org/diary.org")
         (datetree-date (org-entry-get nil "CLOSED" t))
         (date (org-date-to-gregorian datetree-date)))
    (save-window-excursion
      (org-cut-subtree)
      (find-file diary-file)
      (org-datetree-find-date-create date)
      (org-end-of-subtree t)
      (newline)
      (org-paste-subtree 4))))

(defun my:org-datetree-find-date-create-subtree ()
  (interactive)
  (let ((date (org-date-to-gregorian (org-read-date))))
   (org-datetree-find-date-create date 'subtree-at-point)))

(use-package alert
  :ensure t
  :config
  (setq alert-default-style 'libnotify))

(use-package org-alert
  :after (org alert)
  :load-path "~/.emacs.d/packages/org-alert")

(use-package org-super-agenda
  :ensure t
  :after org
  :config
  (org-super-agenda-mode)
  (setq org-super-agenda-groups
        '((:name "Clocking"
                 :log t
                 :pred (lambda (item)
                          (org-find-text-property-in-string 'time-of-day item)))
          (:name "Others"
                 :anything t))))

(use-package geiser :ensure t
  :config
  (setq geiser-chez-binary "chez-scheme")
  (setq geiser-default-implementation 'chez))
(use-package ob-scheme :after org)
(use-package ob-python :after org)
(use-package ob-shell :after org)
(use-package ob-ipython
  :ensure t
  :after org
  :config
  (setq ob-ipython-resources-dir "~/obipy-resources/")
  (remove-hook 'org-mode-hook 'ob-ipython-auto-configure-kernels)
  (advice-add 'ob-babel-execute:ipython :around 'ob-ipython-auto-configure-kernels))
(use-package ob-metapost
  :commands org-babel-execute:metapost
  :load-path "~/.emacs.d/packages/ob-metapost")

(use-package org-bullets
  :ensure t
  :after org
  :init
  (setq org-bullets-bullet-list '("⚫" "○" "∙"))
  :config
  (add-hook 'org-mode-hook (lambda () (org-bullets-mode 1))))

(use-package elfeed
  :ensure t
  :defer t
  :config
  (evil-make-overriding-map elfeed-search-mode-map 'normal))

(use-package elfeed-org
  :ensure t
  :after elfeed
  :config
  (elfeed-org)
  (setq rmh-elfeed-org-files '("~/org/feeds.org")))

(defun my:toggle-line-number ()
  "Toggle line number between relative and nil."
  (interactive)
  (setq display-line-numbers
	(pcase display-line-numbers
	  ('relative nil)
	  (_ 'relative))))

(defun my:eshell-complete ()
  (interactive)
  (pcomplete-std-complete))

(defun my:eshell-hook ()
  (general-def eshell-mode-map
    "<tab>" 'completion-at-point))

(use-package eshell
  :hook (eshell-mode . my:eshell-hook))

(use-package esh-autosuggest
  :ensure t
  :after eshell
  :hook (eshell-mode . esh-autosuggest-mode))

(use-package eshell-z
  :ensure t
  :after eshell)

(use-package em-tramp
  :after (eshell esh-module)
  :config
  (add-to-list 'eshell-modules-list 'eshell-tramp))

(use-package dired
  :config
  (defun my:dired-mode-hide ()
    (dired-hide-details-mode 1))
  (add-hook 'dired-mode-hook 'my:dired-mode-hide)
  (setq dired-dwim-target t))
(use-package all-the-icons
  :ensure t
  :config
  (add-to-list 'all-the-icons-icon-alist
               '("\\.v" all-the-icons-fileicon "coq" :face all-the-icons-red))
  (add-to-list 'all-the-icons-mode-icon-alist
               '(coq-mode all-the-icons-fileicon "coq" :face all-the-icons-red)))
(use-package all-the-icons-dired
  :ensure t
  :after (dired all-the-icons)
  :hook (dired-mode . all-the-icons-dired-mode))

(use-package tex
  :ensure auctex
  :defer t
  :config
  (setq TeX-auto-save t
        TeX-PDF-mode t
        TeX-electric-sub-and-superscript t))

(use-package python
  :defer t
  :config
  (setq python-indent-offset 4)
  (setq python-indent-guess-indent-offset-verbose nil)
  (setq python-shell-completion-native-enable nil)
  (when (executable-find "ipython")
    (setq python-shell-interpreter "ipython")))

(defun my:message-status ()
  "message me"
  (interactive)
  (message
   "%s"
   (concat
    (eyebrowse-mode-line-indicator)
    (propertize (buffer-name (current-buffer)) 'face '(:background "pale green"))
    (propertize evil-mode-line-tag 'face '(:background "sandy brown"))
    (propertize org-mode-line-string 'face '(:background "pale green"))
    (propertize
     (concat
      " "
      (shell-command-to-string "cat /sys/class/power_supply/BAT0/capacity | tr -d '\n'")
      " ") 'face '(:background "sandy brown"))
    (propertize (format-time-string " %y-%m-%d %H:%M") 'face '(:background "pale green")))))

(defun my:other-window-or-buffer ()
  "Switch to other window or buffer"
  (interactive)
  (if (one-window-p) (switch-to-buffer (other-buffer)) (select-window (next-window))))

(use-package general
  :ensure t
  :config
  (general-evil-setup)
  (general-def '(minibuffer-local-map
                 minibuffer-local-ns-map
                 minibuffer-local-completion-map
                 minibuffer-local-must-match-map
                 minibuffer-local-isearch-map
                 ivy-minibuffer-map)
    [escape] 'minibuffer-keyboard-quit)
  (general-iemap
    "," (general-key-dispatch 'self-insert-command
          :timeout 0.20
          :inherit-keymap my:prefix-map))
  (general-mmap
    :prefix ","
    :keymaps 'override
    :prefix-command 'my:prefix-map
    "f" 'counsel-find-file
    "d" 'dired
    "b" 'ivy-switch-buffer
    "g" 'magit-status
    "a" 'org-agenda-list
    "," 'my:other-window-or-buffer
    "r" 'counsel-rg
    "A" 'org-agenda
    "c" 'org-capture
    "s" 'save-buffer
    "S" 'save-some-buffers
    "q q" 'evil-save-and-quit
    "q k" 'kill-emacs
    "/" 'swiper
    "e" 'hydra-eyebrowse/body
    "w" 'hydra-window/body
    "B" 'hydra-buffer/body
    "SPC r" 'counsel-recentf
    "SPC f" 'counsel-fzf
    "h f" 'describe-function
    "h F" 'counsel-describe-face
    "h v" 'describe-variable
    "h c" 'describe-char
    "h m" 'describe-mode
    "h i" 'describe-info
    "h k" 'describe-key
    "h b" 'counsel-descbinds
    "o g" 'org-clock-goto
    "o o" 'org-clock-out
    "o c" 'cfw:open-org-calendar
    "n d" 'narrow-to-defun
    "n s" 'org-narrow-to-subtree
    "n w" 'widen
    "p p" 'projectile-switch-project
    "p c" 'projectile-compile-project
    "p r" 'projectile-recentf
    "p f" 'projectile-find-file)
  (general-mmap
    :prefix "SPC"
    "" nil
    "a" 'align
    "e" 'hydra-expand-region/body)
  (general-nmap org-mode-map
    "gh" 'outline-up-heading
    "gj" 'org-forward-heading-same-level
    "gk" 'org-backward-heading-same-level
    "gl" 'outline-next-visible-heading
    "<" 'org-metaleft
    ">" 'org-metaright
    "t" 'org-todo)
  (general-def org-mode-map
    "M-h" 'org-metaleft
    "M-j" 'org-metadown
    "M-k" 'org-metaup
    "M-l" 'org-metaright
    "M-H" 'org-shiftleft
    "M-J" 'org-shiftdown
    "M-K" 'org-shiftup
    "M-L" 'org-shiftright)
  (general-mmap org-mode-map
    :prefix ";"
    :prefix-command 'my:org-mode-prefix-command
    :prefix-map 'my:org-mode-prefix-map
    "r" 'my:org-refile-to-diary
    "s" 'org-schedule
    "d" 'org-deadline
    "e" 'org-edit-src-code
    "c" 'org-columns
    "l" 'org-insert-link
    "p" 'org-set-property
    "i" 'org-toggle-inline-images
    "o" 'org-open-at-point
    ";" 'org-ctrl-c-ctrl-c
    "k" 'hydra-org-clock/body)
  (general-iemap org-mode-map
   ";" (general-key-dispatch 'self-insert-command
         :timeout 0.20
         :inherit-keymap my:org-mode-prefix-map))
  (general-define-key
   :definer 'minor-mode
   :states '(normal visual)
   :keymaps 'org-src-mode
   :prefix ";"
   "e" 'org-edit-src-exit
   "k" 'org-edit-src-abort)
  (general-define-key
   :definer 'minor-mode
   :states '(normal visual)
   :keymaps 'org-capture-mode
   :prefix ";"
   "e" 'org-capture-finalize
   "w" 'org-capture-refile
   "k" 'org-capture-kill)
  (general-def org-agenda-mode-map
    "S" 'org-agenda-schedule
    "D" 'org-agenda-deadline
    "j" 'org-agenda-next-line
    "k" 'org-agenda-previous-line)
  (general-def 'override
    "M-m" 'my:message-status
    "C-=" 'text-scale-increase
    "C--" 'text-scale-decrease)
  (general-def 'normal coq-mode-map
    "K" 'my:company-coq-doc-search)
  (general-mmap coq-mode-map
    :prefix ";"
    :prefix-command 'my:coq-mode-prefix-command
    :prefix-map 'my:coq-mode-prefix-map
    "g" 'company-coq-proof-goto-point
    "e" 'proof-shell-exit
    "p" 'proof-prf
    "u" 'proof-undo-last-successful-command
    "s" 'proof-find-theorems
    "l" 'proof-layout-windows
    "c" 'proof-query-identifier)
  (general-iemap coq-mode-map
   ";" (general-key-dispatch 'self-insert-command
         :timeout 0.20
         :inherit-keymap my:coq-mode-prefix-map))
  (general-mmap Info-mode-map
    "q" 'Info-exit
    "u" 'Info-up
    "b" 'Info-history-back
    "n" 'Info-next
    "p" 'Info-prev
    "<tab>" 'Info-next-reference
    "S-<tab>" 'Info-prev-reference)
  (general-def 'normal help-mode-map
    "q" 'quit-window)
  (general-def 'emacs cfw:calendar-mode-map
    "d" 'cfw:change-view-day
    "w" 'cfw:change-view-week
    "W" 'cfw:change-view-two-weeks
    "m" 'cfw:change-view-month))

(custom-set-variables
 ;; custom-set-variables was added by Custom.
 ;; If you edit it by hand, you could mess it up, so be careful.
 ;; Your init file should contain only one such instance.
 ;; If there is more than one, they won't work right.
 '(package-selected-packages
   (quote
    (evil-matchit alert org-super-agenda expand-region proof-general highlight-indent-guides ivy-hydra general auctex all-the-icons-dired eshell-z esh-autosuggest elfeed-org elfeed org-bullets ob-ipython geiser lua-mode ccls company-lsp lsp-ui lsp-mode flycheck all-the-icons-ivy counsel hydra which-key rainbow-delimiters evil-surround evil calfw-org calfw magit haskell-mode eyebrowse company-coq company use-package))))
(custom-set-faces
 ;; custom-set-faces was added by Custom.
 ;; If you edit it by hand, you could mess it up, so be careful.
 ;; Your init file should contain only one such instance.
 ;; If there is more than one, they won't work right.
 )
