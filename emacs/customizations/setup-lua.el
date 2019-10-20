;; Shortcuts for lua mode
(require 'bind-map)
(bind-map my-lua-mode-map
  :keys ("s-,")
  :evil-keys (",")
  :evil-states (normal visual)
  :major-modes (lua-mode)
  :bindings ("h" 'lua-search-documentation
             "ee" 'lua-send-current-line
             "ef" 'lua-send-defun
             "eb" 'lua-send-buffer
             "er" 'lua-send-lua-region
             "ll" (lambda ()
                    (interactive)
                    (let ((game-directory (file-name-directory (my-get-closest-pathname "main.lua" 3))))
                      (emamux:run-command (concat "love" " " game-directory))))
             "rq" (lambda ()
                    (interactive)
                    (call-interactively 'lua-kill-process)
                    (call-interactively 'delete-window))
             "rr" 'lua-restart-with-whole-file))

(add-hook 'comint-mode-hook
          (function (lambda ()
  		      (local-set-key (kbd "<up>") 'comint-previous-input)
  		      (local-set-key (kbd "<down>") 'comint-next-input)
                      (local-set-key (kbd "C-l") 'comint-clear-buffer))))
