(setq-default c-basic-offset 4)

;; Treat underscores as part of a symbol for easy searching with */#
(add-hook 'c-mode-common-hook 'superword-mode)

;; Fix indentation of switch statements
(c-set-offset 'case-label '+)

(require 'compile)
(add-hook 'c-mode-hook
          (lambda ()
            (set (make-local-variable 'compile-command)
                 (format
                  "make -C \"%s\""
                  (file-name-directory
                   (my-get-closest-pathname "Makefile" 3))))))

(setq compilation-scroll-output 'first-error)

;; Shortcuts for c mode
(require 'bind-map)
(bind-map my-c-mode-map
  :keys ("s-,")
  :evil-keys (",")
  :evil-states (normal visual)
  :major-modes (c-mode)
  :bindings ("cc" (lambda ()
                    (interactive)
                    (call-interactively 'compile)
                    (select-window (get-buffer-window "*compilation*")))
             "cr" (lambda ()
                    (interactive)
                    (call-interactively 'recompile)
                    (select-window (get-buffer-window "*compilation*")))))
