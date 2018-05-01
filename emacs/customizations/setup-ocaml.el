(let ((opam-share (ignore-errors (car (process-lines "opam" "config" "var" "share")))))
 (when (and opam-share (file-directory-p opam-share))
  ;; Register Merlin
  (add-to-list 'load-path (expand-file-name "emacs/site-lisp" opam-share))
  (autoload 'merlin-mode "merlin" nil t nil)
  ;; Automatically start it in OCaml buffers
  (add-hook 'tuareg-mode-hook 'merlin-mode t)
  (add-hook 'caml-mode-hook 'merlin-mode t)
  ;; Use opam switch to lookup ocamlmerlin binary
  (setq merlin-command 'opam)))

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
