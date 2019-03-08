;;; ob-metapost.el --- Babel Functions for metapost  -*- lexical-binding: t; -*-

;; Copyright (C) 2009-2018 Free Software Foundation, Inc.

;; Author: hawnzug
;; Keywords: literate programming, reproducible research
;; Homepage: https://orgmode.org

;; This file is part of GNU Emacs.

;; GNU Emacs is free software: you can redistribute it and/or modify
;; it under the terms of the GNU General Public License as published by
;; the Free Software Foundation, either version 3 of the License, or
;; (at your option) any later version.

;; GNU Emacs is distributed in the hope that it will be useful,
;; but WITHOUT ANY WARRANTY; without even the implied warranty of
;; MERCHANTABILITY or FITNESS FOR A PARTICULAR PURPOSE.  See the
;; GNU General Public License for more details.

;; You should have received a copy of the GNU General Public License
;; along with GNU Emacs.  If not, see <https://www.gnu.org/licenses/>.

;;; Commentary:

;; Org-Babel support for evaluating metapost source code.
;;
;; 1) there is no such thing as a "session" in metapost
;;
;; 2) we are generally only going to return results of type "file"
;;
;; 3) we are adding the "file" header argument
;;
;; 4) there are no variables (at least for now)

;;; Code:
(require 'ob)

(defvar org-babel-default-header-args:metapost
  '((:results . "file") (:exports . "results"))
  "Default arguments to use when evaluating a dot source block.")

(defun org-babel-execute:metapost (body params)
  "Execute a block of MetaPost code with org-babel.
This function is called by `org-babel-execute-src-block'."
  (let* ((out-file (cdr (or (assq :file params)
                            (error "You need to specify a :file parameter"))))
         (in-file (org-babel-temp-file "metapost-"))
         (cmd (concat "mpost"
                      " " "-s 'outputformat=\"svg\"'"
                      " " (format "-s 'outputtemplate=\"%s\"'" out-file)
                      " " (org-babel-process-file-name in-file))))
    (message cmd)
    (with-temp-file in-file
      (insert (org-babel-expand-body:generic body params)))
    (message "before eval")
    (org-babel-eval cmd "")
    nil))

(defun org-babel-prep-session:metapost (_session _params)
  "Return an error because MetaPost does not support sessions."
  (error "MetaPost does not support sessions"))

(provide 'ob-metapost)



;;; ob-metapost.el ends here
