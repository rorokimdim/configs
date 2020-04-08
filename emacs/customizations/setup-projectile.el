(use-package projectile
  :ensure t
  :config
  (define-key projectile-mode-map (kbd "C-c p") 'projectile-command-map)
  (projectile-mode +1)
  (projectile-clear-known-projects)
  (mapc #'projectile-add-known-project (my-get-workspace-directories)))
