(defun clear-python-buffer ()
  (interactive)
  (let ((old-buffer (buffer-name)))
    (switch-to-buffer "*Python*")
    (comint-clear-buffer)
    (switch-to-buffer old-buffer)))

(use-package lsp-pyright
  :ensure t
  :after python
  :config
  (modify-syntax-entry ?_ "w" python-mode-syntax-table)
  (add-hook 'inferior-python-mode-hook
            (lambda ()
              (local-set-key (kbd "<up>") 'comint-previous-input)
              (local-set-key (kbd "<down>") 'comint-next-input)
              (local-set-key (kbd "C-l") 'comint-clear-buffer)))
  (add-hook 'python-mode-hook
            (lambda ()
              (smartparens-mode)
              (show-smartparens-mode)
              (highlight-parentheses-mode)
              (setq python-indent-offset 4)
              (require 'lsp-pyright)
              (lsp))))

(require 'bind-map)
(bind-map my/python-mode-map
  :keys ("s-,")
  :evil-keys (",")
  :evil-states (normal visual)
  :major-modes (python-mode)
  :bindings ("gd" 'lsp-find-definition
             "gr" 'lsp-find-references
             "hh" 'lsp-ui-doc-show
             "r" (lambda ()
                   (interactive)
                   (call-interactively 'run-python)
                   (call-interactively 'python-shell-switch-to-shell))
             "eb" (lambda ()
                    (interactive)
                    (clear-python-buffer)
                    (call-interactively 'python-shell-send-buffer))
             "er" (lambda ()
                    (interactive)
                    (clear-python-buffer)
                    (call-interactively 'python-shell-send-region))
             "ec" 'clear-python-buffer
             "ee" 'python-shell-send-statement
             "ef" 'python-shell-send-defun))
