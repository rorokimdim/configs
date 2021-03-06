(use-package go-mode
  :ensure t
  :init
  :config
  :hook ((go-mode . lsp-deferred)
         (before-save . lsp-format-buffer)
         (before-save . lsp-organize-imports)
         ))

(defun my/go-build ()
  (interactive)
  (emamux:run-command "go build" (ffip-get-project-root-directory)))

(defun my/go-run ()
  (interactive)
  (emamux:run-command (concat "clear && go run " (buffer-file-name))
                      (ffip-get-project-root-directory)))

(defun my/go-run-tests ()
  (interactive)
  (emamux:run-command "clear && go test -v"
                      (file-name-directory buffer-file-name)))

(defun my/clear-terminal ()
  (interactive)
  (emamux:run-command "clear" (ffip-get-project-root-directory)))

(require 'bind-map)
(bind-map my/golang-mode-map
  :keys ("s-,")
  :evil-keys (",")
  :evil-states (normal visual)
  :major-modes (go-mode)
  :bindings ("dd" 'my/lsp-ui-doc-toggle
             "gt" 'lsp-find-type-definition
             "gd" 'lsp-find-definition
             "gr" 'lsp-find-references
             "rr" 'my/go-run
             "re" 'emamux:close-panes
             "rl" 'my/clear-terminal
             "rt" 'my/go-run-tests
             ))

(add-hook 'go-mode-hook (lambda () (setq tab-width 4)))
