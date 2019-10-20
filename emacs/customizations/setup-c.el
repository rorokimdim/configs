(setq-default c-basic-offset 4)

;; Treat underscores as part of a symbol for easy searching with */#
(add-hook 'c-mode-common-hook 'superword-mode)
(add-hook 'c++-mode-common-hook 'superword-mode)

(use-package lsp-mode :commands lsp :ensure t)
(use-package lsp-ui :commands lsp-ui-mode :ensure t)
(use-package company-lsp
  :ensure t
  :commands company-lsp
  :config
  (setq lsp-ui-doc-enable nil)
  (setq lsp-eldoc-enable-hover nil)
  (setq lsp-enable-file-watchers nil)
  (setq lsp-enable-symbol-highlighting nil)
  (setq lsp-eldoc-enable-signature-help nil)
  (push 'company-lsp company-backends))


(use-package ccls
  :ensure t
  :config
  (setq ccls-executable "ccls")
  :hook ((c-mode c++-mode objc-mode) .
         (lambda () (require 'ccls) (lsp))))

;; Fix indentation of switch statements
(c-set-offset 'case-label '+)

(require 'compile)

;; Assume I am using cmake
(dolist (hook '(c-mode-hook c++-mode-hook))
  (add-hook hook #'smartparens-mode)
  (add-hook hook 'show-smartparens-mode)
  (add-hook hook 'highlight-parentheses-mode)
  (add-hook hook 'rainbow-delimiters-mode)
  (add-hook hook
            (lambda ()
              (set (make-local-variable 'compile-command)
                   (format
                    "make -C \"%sbuild\""
                    (file-name-directory
                     (my-get-closest-pathname "CMakeLists.txt" 3)))))))

(setq compilation-scroll-output 'first-error)

;; Shortcuts for c/cpp mode
(require 'bind-map)
(bind-map my-cxx-mode-map
  :keys ("s-,")
  :evil-keys (",")
  :evil-states (normal visual)
  :major-modes (c-mode c++-mode)
  :bindings ("cc" (lambda ()
                    (interactive)
                    (call-interactively 'compile)
                    (select-window (get-buffer-window "*compilation*")))
             "r" (lambda ()
                   (interactive)
                   (let ((root-dir (file-name-directory (my-get-closest-pathname "CMakeLists.txt" 3))))
                     (shell-command
                      (format "make -C %s/build && %s/build/main" root-dir root-dir))))
             "h" 'lsp-describe-thing-at-point
             "m" (lambda ()
                   (interactive)
                   (let ((thing (thing-at-point 'symbol)))
                     (emamux:run-command (concat "cppman" " " thing " && " "exit"))
                     (emamux:select-pane (emamux:get-runner-pane-id))))
             "dd" 'dash-at-point
             "dl" 'dash-at-point-with-docset
             "en" 'flymake-goto-next-error
             "ep" 'flymake-goto-prev-error
             ))
