;; -*- lexical-binding: t; -*-

(require 'org)
(require 'org-capture)
(require 'citar)

(defun my:citar-notes-get-notes (&optional keys)
  (let ((ht (make-hash-table :test 'equal)))
    (org-with-file-buffer "~/org/inbox.org"
      (org-element-cache-map
       (lambda (el)
         (when-let* ((key (org-element-property :BIBTEX_KEY el))
                     ((or (null keys) (member key keys))))
           (push (concat (int-to-string (org-element-begin el))
                         "#" (org-element-property :title el))
                 (gethash key ht))))
       :granularity 'headline))
    ht))

(defun my:citar-notes-has-notes ()
  (let ((ht (my:citar-notes-get-notes)))
    (lambda (citekey) (and (gethash citekey ht) t))))

(defun my:citar-notes-open-note (id)
  (find-file "~/org/inbox.org")
  (string-match (rx bos (group (+ digit)) ?#) id)
  (goto-char (string-to-number (match-string 1 id)))
  (org-fold-show-context))

(defun my:citar-notes-create-note (key entry)
  (let ((org-capture-templates
         `(("r" "Reading" entry (file+olp "~/org/inbox.org" "Readings")
            ,(concat "* " (assoc-default "title" entry)
                     "\n:PROPERTIES:\n:CREATED:  %U\n:BIBTEX_KEY:  " key
                     "\n:END:\n%?")
            :prepend t))))
    (org-capture nil "r")))

(setf (alist-get 'my:citar-notes citar-notes-sources)
      '(:name "Citar Org Notes"
        :category file
        :items my:citar-notes-get-notes
        :hasitems my:citar-notes-has-notes
        :open my:citar-notes-open-note
        :create my:citar-notes-create-note))

;; (setopt citar-notes-source 'my:citar-notes)
;; (length (gethash "CCHM18" (my:citar-notes-get-notes (list))))

(provide 'my:citar-notes)
