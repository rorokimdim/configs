(autoload 'merlin-mode "merlin" nil t nil)
(add-hook 'tuareg-mode-hook 'merlin-mode t)
(add-hook 'tuareg-interactive-mode-hook 'merlin-mode t)
(add-hook 'caml-mode-hook 'merlin-mode t)

(when (file-exists-p "~/.opam/system/share/emacs/site-lisp/tuareg-site-file.el")
  (load "~/.opam/system/share/emacs/site-lisp/tuareg-site-file"))

;; Shortcuts for ocaml mode
(require 'bind-map)
(bind-map my-ocaml-mode-map
  :keys ("s-,")
  :evil-keys (",")
  :evil-states (normal visual)
  :major-modes (tuareg-mode
                caml-mode
                merlin-mode)
  :bindings ("r" 'tuareg-run-ocaml
             "ee" 'tuareg-eval-phrase
             "er" 'tuareg-eval-region
             "eb" 'tuareg-eval-buffer))

;; Shorcuts for ocaml repl
(add-hook 'tuareg-interactive-mode-hook
  (lambda ()
    (define-key tuareg-interactive-mode-map (kbd "C-l") 'comint-clear-buffer)))
