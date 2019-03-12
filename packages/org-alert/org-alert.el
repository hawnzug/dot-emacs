(require 'alert)
(require 'org)
(require 'seq)
(require 'pcase)
(require 'cl-lib)
(require 'cl-extra)

(defun org-alert-entry ()
  (when (pcase-let ((`(_ _ _ . ,today-dmy) (decode-time)))
          (seq-some
           (pcase-lambda (`(_ _ _ . ,dmy))
             (cl-every '= (seq-take dmy 3) today-dmy))
           (seq-filter
            'identity
            (seq-map
             (lambda (p)
               (when-let (ts (org-entry-get (point) p))
                 (org-parse-time-string ts)))
             '("SCHEDULED" "DEADLINE" "TIMESTAMP")))))
    (seq-elt (org-heading-components) 4)))

(defun org-alert-check ()
  (interactive)
  (dolist (title (seq-filter
                  'identity
                  (org-map-entries 'org-alert-entry "TODO=\"TODO\"" '("~/org/sjtu.org"))))
    (alert title :title "Todo")))

(defun org-alert-enable ()
  "Enable the notification timer.  Cancels existing timer if running."
  (interactive)
  (org-alert-disable)
  (run-at-time 0 600 'org-alert-check))

(defun org-alert-disable ()
  "Cancel the running notification timer."
  (interactive)
  (dolist (timer timer-list)
    (when (eq (seq-elt timer 5) 'org-alert-check)
      (cancel-timer timer))))

(provide 'org-alert)
