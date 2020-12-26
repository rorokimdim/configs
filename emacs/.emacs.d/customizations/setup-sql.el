;; Truncate long lines
(add-hook 'sql-interactive-mode-hook
  (lambda () (toggle-truncate-lines t)))

;; Capitalize keywords in an interactive session (e.g. psql)
(add-hook 'sql-interactive-mode-hook 'sqlup-mode)
(add-hook 'sql-mode-hook 'sqlup-mode)

;; Shortcuts for sql mode
(require 'bind-map)
(bind-map my/sql-mode-map
  :evil-keys (",")
  :evil-states (normal visual)
  :major-modes (sql-mode)
  :bindings ("fb" 'sqlup-capitalize-keywords-in-buffer
             "fr" 'sqlup-capitalize-keywords-in-region
             "ee" 'sql-send-paragraph
             "eb" 'sql-send-buffer))
