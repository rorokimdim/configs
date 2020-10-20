(defun clear-python-buffer ()
  (let ((old-buffer (buffer-name)))
    (switch-to-buffer "*Python*")
    (comint-clear-buffer)
    (switch-to-buffer old-buffer)))

(defun elpy-goto-definition-or-rgrep ()
  "Go to the definition of the symbol at point, if found. Otherwise, run `elpy-rgrep-symbol'."
  (interactive)
  (ring-insert find-tag-marker-ring (point-marker))
  (condition-case nil (elpy-goto-definition-other-window)
    (error (elpy-rgrep-symbol
            (concat "\\(def\\|class\\)\s" (thing-at-point 'symbol) "(")))))

(use-package elpy
  :ensure t
  :after python
  :config
  (setq-default py-shell-name "ipython")
  (setq-default py-which-bufname "IPython")
  (setq elpy-rpc-timeout nil)  ; Note: Remember to pip install rope/jedi
  (setq py-force-py-shell-name-p t)
  (modify-syntax-entry ?_ "w" python-mode-syntax-table)
  (evil-leader/set-key-for-mode 'python-mode
    "r" (lambda ()
          (interactive)
          (call-interactively 'run-python)
          (call-interactively 'python-shell-switch-to-shell))
    "eb" (lambda ()
           (interactive)
           (call-interactively 'elpy-shell-send-region-or-buffer)
           (clear-python-buffer))
    "er" (lambda ()
           (interactive)
           (call-interactively 'elpy-shell-send-region-or-buffer)
           (clear-python-buffer))
    "ec" 'clear-python-buffer
    "ef" 'python-shell-send-defun
    "gd" 'elpy-goto-definition-or-rgrep
    "pd" 'elpy-doc
    "ve" 'venv-workon)
  (add-hook 'inferior-python-mode-hook
            (lambda ()
              (local-set-key (kbd "<up>") 'comint-previous-input)
              (local-set-key (kbd "<down>") 'comint-next-input)
              (local-set-key "\C-l" 'comint-clear-buffer)))
  (add-hook 'python-mode-hook
            (lambda ()
              (smartparens-mode)
              (show-smartparens-mode)
              (highlight-parentheses-mode)
              (rainbow-delimiters-mode)
              (setq python-indent-offset 4))))

;; For some reason doing this inside use-package block above doesn't always work.
;; Do it here instead.
(elpy-enable)

;; Setup virtualenvwrapper
(require 'virtualenvwrapper)
(venv-initialize-interactive-shells)
(venv-initialize-eshell)
(setq venv-location "~/venvs")
(setenv "WORKON_HOME" "~/venvs")
