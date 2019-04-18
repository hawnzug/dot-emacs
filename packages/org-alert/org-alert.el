(require 'alert)
(require 'org)
(require 'seq)
(require 'pcase)

(defun org-alert--entry ()
  (defun org-alert--get-ts ()
    (when-let (ts (org-entry-get (point) "alert"))
      (apply #'encode-time (org-parse-time-string ts))))
  (when-let (ts (org-alert--get-ts))
    (when (time-less-p ts (time-add (current-time) (* 60 3)))
      (list
       (seq-elt (org-heading-components) 4)
       (org-get-category)
       (format-time-string "%H:%M" ts)))))

(defun org-alert-check ()
  (interactive)
  (pcase-dolist
      (`(,entry ,category ,time)
       (seq-filter
        #'identity
        (org-map-entries #'org-alert--entry "TODO=\"TODO\"" '("~/org/sjtu.org"))))
    (alert entry :title (concat category " " "[" time "]"))))

(defun org-alert-enable ()
  "Enable the notification timer.  Cancels existing timer if running."
  (interactive)
  (org-alert-disable)
  (run-at-time 0 300 'org-alert-check))

(defun org-alert-disable ()
  "Cancel the running notification timer."
  (interactive)
  (dolist (timer timer-list)
    (when (eq (seq-elt timer 5) 'org-alert-check)
      (cancel-timer timer))))

(provide 'org-alert)
