(defconst haskell-mode-syntax-table
  (let ((table (make-syntax-table)))
    (modify-syntax-entry ?\  " " table)
    (modify-syntax-entry ?\t " " table)
    (modify-syntax-entry ?\" "\"" table)
    (modify-syntax-entry ?\' "_" table)
    (modify-syntax-entry ?_  "_" table)
    (modify-syntax-entry ?\( "()" table)
    (modify-syntax-entry ?\) ")(" table)
    (modify-syntax-entry ?\[  "(]" table)
    (modify-syntax-entry ?\]  ")[" table)

    (modify-syntax-entry ?\{  "(}1nb" table)
    (modify-syntax-entry ?\}  "){4nb" table)
    (modify-syntax-entry ?-  ". 123" table)
    (modify-syntax-entry ?\n ">" table)

    (modify-syntax-entry ?\` "$`" table)

    (mapc (lambda (x)
            (modify-syntax-entry x "." table))
          "!#$%&*+./:<=>?@^|~,;\\")

    table))

(setq haskell-font-lock-keywords
      (let* ((keywords '("anyclass" "as" "case" "class" "data" "default" "deriving" "do"
                         "else" "hiding" "if" "import" "in" "infix" "infixl" "infixr"
                         "instance" "let" "module" "mdo" "newtype" "of" "pattern" "proc"
                         "rec" "signature" "then" "type" "qualified" "via" "where"))
             (keywords-regexp (regexp-opt keywords 'words)))
        `((,keywords-regexp . font-lock-keyword-face))))

(define-derived-mode haskell-mode prog-mode "Simple Haskell Mode"
  :syntax-table haskell-mode-syntax-table
  (setq font-lock-defaults '((haskell-font-lock-keywords))))

(provide 'haskell-mode)

