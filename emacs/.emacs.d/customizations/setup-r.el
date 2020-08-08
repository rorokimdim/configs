;; Shortcuts for r mode
(require 'bind-map)
(bind-map my-r-mode-map
  :keys ("s-,")
  :evil-keys (",")
  :evil-states (normal visual)
  :major-modes (ess-r-mode)
  :bindings ("r" 'ess-switch-to-inferior-or-script-buffer
             "eb" 'ess-eval-buffer
             "ee" 'ess-eval-line
             "ef" 'ess-eval-function
             "er" 'ess-eval-region))

(with-eval-after-load 'ess-r-mode
  (define-key inferior-ess-mode-map (kbd "C-l") 'comint-clear-buffer)
  (define-key inferior-ess-mode-map (kbd "<up>") 'comint-previous-input)
  (define-key inferior-ess-mode-map (kbd "<down>") 'comint-next-input))
