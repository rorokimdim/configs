(defun clear-python-buffer ()
  (let ((old-buffer (buffer-name)))
    (switch-to-buffer "*Python*")
    (comint-clear-buffer)
    (switch-to-buffer old-buffer)))

(use-package lsp-python-ms
  :ensure t
  :after python
  :init (setq lsp-python-ms-auto-install-server t)
  :config
  (setq-default py-shell-name "ipython")
  (setq-default py-which-bufname "IPython")
  (setq py-force-py-shell-name-p t)
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
              (require 'lsp-python-ms)
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
                    (call-interactively 'python-shell-send-buffer)
                    (clear-python-buffer))
             "er" (lambda ()
                    (interactive)
                    (call-interactively 'python-shell-send-region)
                    (clear-python-buffer))
             "ef" 'python-shell-send-defun))
