(setq-default c-basic-offset 4)

;; Treat underscores as part of a symbol for easy searching with */#
(add-hook 'c-mode-common-hook 'superword-mode)
(add-hook 'c++-mode-common-hook 'superword-mode)

;; Fix indentation of switch statements
(c-set-offset 'case-label '+)

(require 'compile)

;; Assume I am using cmake
(dolist (hook '(c-mode-hook c++-mode-hook))
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
             ))
