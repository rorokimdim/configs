(add-hook 'css-mode-hook 'skewer-css-mode)
(add-hook 'css-mode-hook 'emmet-mode)
(add-hook 'css-mode-hook (lambda ()
                           (setq css-indent-offset 2)))

(require 'bind-map)
(bind-map my/css-mode-map
  :keys ("s-,")
  :evil-keys (",")
  :evil-states (normal visual)
  :major-modes (css-mode)
  :bindings ("eb" 'skewer-css-eval-buffer
             "ee" 'skewer-css-eval-current-declaration
             "ef" 'skewer-css-eval-current-rule
             "sc" 'list-skewer-clients
             ))
