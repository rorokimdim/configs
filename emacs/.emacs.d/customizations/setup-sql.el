;; Truncate long lines
(add-hook 'sql-interactive-mode-hook
          (lambda () (toggle-truncate-lines t)))

;; Shortcuts for sql mode
(require 'bind-map)
(bind-map my/sql-mode-map
  :evil-keys (",")
  :evil-states (normal visual)
  :major-modes (sql-mode)
  :bindings ("ee" 'sql-send-paragraph
             "eb" 'sql-send-buffer))
