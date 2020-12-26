(when (eq system-type 'darwin)
  ;; ls --dired is not supported in mac
  (setq dired-use-ls-dired nil)

  ;; OSX packages to install
  (defvar my/osx-packages
    '(osx-dictionary))

  (dolist (p my/osx-packages)
    (when (not (package-installed-p p))
      (package-install p))))
