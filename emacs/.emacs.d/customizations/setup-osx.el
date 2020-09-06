(when (eq system-type 'darwin)
  ;; ls --dired is not supported in mac
  (setq dired-use-ls-dired nil)

  ;; OSX packages to install
  (defvar my-osx-packages
    '(osx-dictionary))

  (dolist (p my-osx-packages)
    (when (not (package-installed-p p))
      (package-install p)))

  ;; Make shell commands work in Mac.
  ;; https://github.com/purcell/exec-path-from-shell
  (when (memq window-system '(mac ns))
    (when (not (package-installed-p 'exec-path-from-shell))
      (package-install 'exec-path-from-shell))
    (exec-path-from-shell-initialize)
    (exec-path-from-shell-copy-envs '("PATH"))))
