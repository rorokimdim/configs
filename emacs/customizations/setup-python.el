(use-package elpy
  :ensure t
  :after python
  :config
  (setq-default py-shell-name "ipython")
  (setq-default py-which-bufname "IPython")
  (setq py-force-py-shell-name-p t)
  (evil-leader/set-key-for-mode 'python-mode
    "r" (lambda ()
          (interactive)
          (run-python)
          (switch-to-buffer "*Python*")))
  (elpy-enable))
