(defun my-projectile-refresh-projects ()
  "Refreshes known projects in projectile."
  (interactive)
  (projectile-clear-known-projects)
  (mapc #'projectile-add-known-project (my-get-workspace-directories)))

(use-package projectile
  :ensure t
  :diminish
  :config
  (define-key projectile-mode-map (kbd "C-c p") 'projectile-command-map)
  (projectile-mode +1)
  (my-projectile-refresh-projects))
