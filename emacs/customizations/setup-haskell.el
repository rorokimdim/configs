(autoload 'ghc-init "ghc" nil t)
(autoload 'ghc-debug "ghc" nil t)
(add-hook 'haskell-mode-hook (lambda () (ghc-init)))

(with-eval-after-load 'company
  (add-to-list 'company-backends 'company-ghc))
