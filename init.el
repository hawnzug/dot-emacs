(let ((file-name-handler-alist nil))
  (load "~/.config/emacs/config"))
;; (load "~/.config/emacs/test-config")

(setq custom-file "~/.config/emacs/emacs-custom.el")
(load custom-file)

