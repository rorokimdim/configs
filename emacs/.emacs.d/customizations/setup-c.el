(setq-default c-basic-offset 4)

;; Treat underscores as part of a symbol for easy searching with */#
(add-hook 'c-mode-common-hook 'superword-mode)

(add-to-list 'dash-at-point-mode-alist '(c-mode . "c,glib,ncurses,gsl"))

;; Fix indentation of switch statements
(c-set-offset 'case-label '+)

(require 'compile)

(dolist (hook '(c-mode-hook))
  (add-hook hook #'smartparens-mode)
  (add-hook hook 'show-smartparens-mode)
  (add-hook hook 'highlight-parentheses-mode)
  (add-hook hook
            (lambda ()
              (set (make-local-variable 'compile-command) "scons -D"))))

(setq compilation-scroll-output 'first-error)

;; Shortcuts for c/cpp mode
(require 'bind-map)
(bind-map my/c-mode-map
  :keys ("s-,")
  :evil-keys (",")
  :evil-states (normal visual)
  :major-modes (c-mode)
  :bindings ("cc" (lambda ()
                    (interactive)
                    (call-interactively 'recompile)
                    (select-window (get-buffer-window "*compilation*")))
             "r" (lambda ()
                   (interactive)
                   (emamux:run-command "scons -D run && exit")
                   (emamux:select-pane (emamux:get-runner-pane-id)))
             "dd" 'dash-at-point
             "dl" 'dash-at-point-with-docset
             "en" 'flymake-goto-next-error
             "ep" 'flymake-goto-prev-error))
