(use-package elpy
  :ensure t
  :after python
  :config
  (setq-default py-shell-name "ipython")
  (setq-default py-which-bufname "IPython")
  (setq elpy-rpc-timeout nil)  ; Note: Remember to pip install rope/jedi
  (setq py-force-py-shell-name-p t)
  (modify-syntax-entry ?_ "w" python-mode-syntax-table)
  (evil-leader/set-key-for-mode 'python-mode
    "r" 'run-python
    "R" (lambda ()
          (interactive)
          (call-interactively 'run-python)
          (call-interactively 'python-shell-switch-to-shell))
    "eb" 'elpy-shell-send-region-or-buffer
    "er" 'elpy-shell-send-region-or-buffer
    "ec" (lambda ()
           (interactive)
           (let ((old-buffer (buffer-name)))
             (switch-to-buffer "*Python*")
             (comint-clear-buffer)
             (switch-to-buffer old-buffer)))
    "ef" 'python-shell-send-defun
    "gd" 'elpy-goto-definition
    "pd" 'elpy-doc
    "pt" 'my-python-add-breakpoint)
  (add-hook 'inferior-python-mode-hook
            (lambda ()
              (local-set-key "\C-l" 'comint-clear-buffer)))
  (add-hook 'python-mode-hook
            (lambda ()
              (rainbow-delimiters-mode)
              (highlight-parentheses-mode)
              (setq python-indent-offset 4))))

;; For some reason doing this inside use-package block above doesn't always work.
;; Do it here instead.
(elpy-enable)
