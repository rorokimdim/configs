(use-package centaur-tabs
  :demand
  :config
  (centaur-tabs-mode t)
  (centaur-tabs-group-by-projectile-project)
  (setq
   centaur-tabs-set-icons nil
   centaur-tabs-gray-out-icons nil
   centaur-tabs-modified-marker "*"
   centaur-tabs-set-modified-marker t
   centaur-tabs-cycle-scope 'tabs)
  :bind
  (:map evil-normal-state-map
        ("g j" . centaur-tabs-ace-jump)
        ("g t" . centaur-tabs-forward)
        ("g T" . centaur-tabs-backward))
  :hook
  (dired-mode . centaur-tabs-local-mode)
  (neotree-mode . centaur-tabs-local-mode))

(defun centaur-tabs-hide-tab (x)
  (let ((name (format "%s" x)))
    (or
     (string-prefix-p "*" name)
     (and (string-prefix-p "magit" name)
          (not (file-name-extension name))))))

(defun my/fix-centaur-tabs ()
  (centaur-tabs-mode -1)
  (centaur-tabs-mode))

(add-hook 'after-make-frame-functions
          (lambda (frame)
            (with-selected-frame frame
              (my/fix-centaur-tabs)))
          (my/fix-centaur-tabs))
