(defun anylink ()
  (interactive)
  (cond
   ((eq major-mode 'org-mode) (org-open-at-point))
   ((eq major-mode 'org-agenda-mode) (org-agenda-open-link))))
