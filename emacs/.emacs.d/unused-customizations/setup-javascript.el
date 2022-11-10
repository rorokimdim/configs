(add-to-list 'auto-mode-alist '("\\.js\\'" . js2-mode))

(add-hook 'js2-mode-hook 'skewer-mode)

(require 'bind-map)
(bind-map my/js-mode-map
  :keys ("s-,")
  :evil-keys (",")
  :evil-states (normal visual)
  :major-modes (js2-mode)
  :bindings ("eb" 'skewer-load-buffer
             "ee" 'skewer-eval-last-expression
             "ef" 'skewer-eval-defun
             "r" 'skewer-repl
             "sr" 'run-skewer
             "sc" 'list-skewer-clients
             ))
