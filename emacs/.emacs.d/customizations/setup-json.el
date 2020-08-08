;; Shortcuts for json mode
(require 'bind-map)
(bind-map my-json-mode-map
  :evil-keys (",")
  :evil-states (normal visual)
  :major-modes (json-mode)
  :bindings ("r" 'jq-interactively))

(add-hook 'json-mode-hook (lambda () (setq js-indent-level 2)))
