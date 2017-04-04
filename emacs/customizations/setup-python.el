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
    "r" (lambda ()
          (interactive)
          (run-python)
          (switch-to-buffer "*Python*"))
    "gd" 'elpy-goto-definition
    "pd" 'elpy-doc
    "pt" 'my-python-add-breakpoint)
  (add-hook 'python-mode-hook
            (lambda ()
              (rainbow-delimiters-mode)
              (highlight-parentheses-mode)
              (setq python-indent-offset 4))))

;; For some reason doing this inside use-package block above doesn't always work.
;; Do it here instead.
(elpy-enable)
