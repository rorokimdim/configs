(setq-default c-basic-offset 4)

;; Treat underscores as part of a symbol for easy searching with */#
(add-hook 'c-mode-common-hook 'superword-mode)

(add-to-list 'dash-at-point-mode-alist '(c-mode . "c,glib,ncurses,gsl"))

;; Fix indentation of switch statements
(c-set-offset 'case-label '+)

(dolist (hook '(c-mode-hook))
  (add-hook hook #'smartparens-mode)
  (add-hook hook 'show-smartparens-mode)
  (add-hook hook 'highlight-parentheses-mode))

(setq compilation-scroll-output 'first-error)

(defun scons/run ()
  (interactive)
  (my/tmux-run-in-buffer-directory "scons" "SConstruct" 4))

(defun scons/run-clean ()
  (interactive)
  (my/tmux-run-in-buffer-directory "scons -c *" "SConstruct" 4))

(defun scons/run-main ()
  (interactive)
  (my/tmux-run-in-buffer-directory "scons main" "SConstruct" 4))

(defun scons/run-test ()
  (interactive)
  (my/tmux-run-in-buffer-directory "scons test" "SConstruct" 4))

;; Shortcuts for c/cpp mode
(require 'bind-map)
(bind-map my/c-mode-map
  :keys ("s-,")
  :evil-keys (",")
  :evil-states (normal visual)
  :major-modes (c-mode)
  :bindings ("cc" 'scons/run
             "rr" 'scons/run
             "rc" 'scons/run-clean
             "rm" 'scons/run-main
             "rt" 'scons/run-test
             "dd" 'dash-at-point
             "dl" 'dash-at-point-with-docset
             "en" 'flymake-goto-next-error
             "ep" 'flymake-goto-prev-error))
