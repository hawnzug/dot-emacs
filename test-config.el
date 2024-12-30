;;;; Package Management
(require 'use-package)

(dolist (path (directory-files package-user-dir))
  (when-let (((not (member path '("." ".." "archives" "gnupg"))))
             (abspath (expand-file-name path package-user-dir))
             ((file-directory-p abspath)))
    (add-to-list 'load-path abspath)))

(use-package nerd-icons
  :ensure t
  :init
  (require 'nerd-icons-autoloads))
