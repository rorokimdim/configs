(add-hook 'html-mode-hook 'skewer-html-mode)
(add-hook 'html-mode-hook 'emmet-mode)

(require 'bind-map)
(bind-map my-html-mode-map
  :keys ("s-,")
  :evil-keys (",")
  :evil-states (normal visual)
  :major-modes (mhtml-mode)
  :bindings ("eb" 'skewer-load-buffer
             "ee" 'skewer-html-eval-tag
             "sc" 'list-skewer-clients
             ))

(with-eval-after-load 'mhtml-mode
  (define-key emmet-mode-keymap (kbd "C-C .") 'emmet-expand-line))
