(require 'go-mode)
(require 'go-guru)

(add-hook 'go-mode-hook 'go-eldoc-setup)

(add-hook 'go-mode-hook
          (lambda ()
            (add-hook 'before-save-hook 'gofmt-before-save)
            (setq tab-width 4)
            (setq indent-tabs-mode 1)
            (setq compile-command "go build")
            (add-to-list 'company-backends 'company-go)))

;; Shortcuts for go mode
(require 'bind-map)
(bind-map my-go-mode-map
  :keys ("s-,")
  :evil-keys (",")
  :evil-states (normal visual)
  :major-modes (go-mode)
  :bindings ("gd" 'go-guru-definition
             "i" 'go-import-add
             "cc" (lambda ()
                    (interactive)
                    (call-interactively 'recompile)
                    (select-window (get-buffer-window "*compilation*"))
                    )))
