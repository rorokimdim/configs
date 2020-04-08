(use-package centaur-tabs
  :demand
  :config
  (centaur-tabs-mode t)
  (centaur-tabs-group-by-projectile-project)
  :bind
  (:map evil-normal-state-map
        ("g t" . centaur-tabs-forward)
        ("g T" . centaur-tabs-backward))
  :hook
  (ranger-mode . centaur-tabs-local-mode)
  (neotree-mode . centaur-tabs-local-mode))

(defun centaur-tabs-hide-tab (x)
  (let ((name (format "%s" x)))
    (or
     (string-prefix-p "*" name))))
