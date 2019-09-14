;;;;
;; Clojure
;;;;

;; This is useful for working with camel-case tokens, like names of
;; Java classes (e.g. JavaClassName)
(add-hook 'clojure-mode-hook 'subword-mode)

;; A little more syntax highlighting
(require 'clojure-mode-extra-font-locking)

;; Use clojure mode for other extensions
(add-to-list 'auto-mode-alist '("\\.edn$" . clojure-mode))
(add-to-list 'auto-mode-alist '("\\.closhrc$" . clojure-mode))
(add-to-list 'auto-mode-alist '("\\.cljs.*$" . clojurescript-mode))

;; Shortcuts for clojure mode
(require 'bind-map)
(bind-map my-clj-cljs-mode-map
  :keys ("s-,")
  :evil-keys (",")
  :evil-states (normal visual)
  :major-modes (clojure-mode
                clojurescript-mode)
  :bindings ("cc" 'spiral-connect
             "ct" 'spiral-connect-to
             "r"  'spiral-switch-to-repl-buffer
             "eb" 'spiral-eval-buffer
             "ee" (lambda ()
                    (interactive)
                    (forward-char)
                    (spiral-eval-last-sexp))
             "ef" 'spiral-eval-top-level-form))

(with-eval-after-load 'spiral-repl
  (custom-set-variables
   '(spiral-automatic-ns-sync (quote do-it-without-notify)))
  (define-key spiral-repl-mode-map (kbd "C-l") 'spiral-repl-clear-buffer)
  (define-key spiral-repl-mode-map (kbd "<up>") 'spiral-repl-previous-input)
  (define-key spiral-repl-mode-map (kbd "<down>") 'spiral-repl-next-input))

(defun spiral-repl--clear-region (start end)
  (mapc #'delete-overlay (overlays-in start end))
  (delete-region start end))

(defun spiral-repl-clear-buffer ()
  "Clears spiral's REPL buffer."
  (interactive)
  (let ((inhibit-read-only t))
    (spiral-repl--clear-region (point-min) spiral-repl-prompt-start-mark)
    (spiral-repl--clear-region spiral-repl-transient-text-start-mark spiral-repl-transient-text-end-mark)
    (when (< (point) spiral-repl-input-start-mark)
      (goto-char spiral-repl-input-start-mark))
    (recenter t)))
