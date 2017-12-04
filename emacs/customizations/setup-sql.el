;; Truncate long lines
(add-hook 'sql-interactive-mode-hook
  (lambda () (toggle-truncate-lines t)))

;; Capitalize keywords in an interactive session (e.g. psql)
(add-hook 'sql-interactive-mode-hook 'sqlup-mode)

;; Shortcuts for sql mode
(require 'evil-leader)
(evil-leader/set-key-for-mode 'sql-mode
  "ee" 'sql-send-paragraph
  "eb" 'sql-send-buffer)
