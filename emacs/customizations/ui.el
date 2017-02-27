;; Turn off the menu bar at the top of each frame because it's distracting
(menu-bar-mode -1)

;; Get rid of useless tool-bar in gui mode
(when (fboundp 'tool-bar-mode)
  (tool-bar-mode -1))

;; Don't show native OS scroll bars for buffers because they're redundant
(when (fboundp 'scroll-bar-mode)
  (scroll-bar-mode -1))

;; Focus on help window when opened
(setq help-window-select t)

;; Color Themes
(load-theme 'wombat t)

;; Emacs gui fame size
(setq initial-frame-alist '((top . 0) (left . 0) (width . 202) (height . 60)))

;; These settings relate to how emacs interacts with your operating system
(setq ;; makes killing/yanking interact with the clipboard
      select-enable-clipboard t
      select-enable-primary t

      ;; Save clipboard strings into kill ring before replacing them.
      ;; When one selects something in another program to paste it into Emacs,
      ;; but kills something in Emacs before actually pasting it,
      ;; this selection is gone unless this variable is non-nil
      save-interprogram-paste-before-kill t

      ;; Shows all options when running apropos. For more info,
      ;; https://www.gnu.org/software/emacs/manual/html_node/emacs/Apropos.html
      apropos-do-all t

      ;; Mouse yank commands yank at point instead of at click.
      mouse-yank-at-point t)

;; No cursor blinking, it's distracting
(blink-cursor-mode 0)

;; Full path in title bar
(setq-default frame-title-format "%b (%f)")

;; Show column number
(column-number-mode t)

;; Set fring color to background color
(set-face-attribute 'fringe nil
                      :foreground (face-foreground 'default)
                      :background (face-background 'default))

;; Don't pop up font menu
(global-set-key (kbd "s-t") '(lambda () (interactive)))

;; No bell
(setq ring-bell-function 'ignore)

;; Override quit-window to kill buffer and window
(defun quit-window (&optional a b)
 "Kills buffer + window on quit-window."
 (interactive)
 (kill-buffer-and-window))

;; Set custom faces
(custom-set-faces
 '(ido-subdir ((t (:foreground "dark cyan"))))
 '(persp-selected-face ((t (:foreground "dark cyan" :weight bold)))))

;; Change neotree width
(custom-set-variables
   '(neo-window-width 50))

;; Enable mouse support on terminal
(unless window-system
  (require 'mouse)
  (xterm-mouse-mode t))

;; Cleanup my mode-line
(eval-after-load "projectile" '(diminish 'projectile-mode))
(eval-after-load "undo-tree" '(diminish 'undo-tree-mode))
(eval-after-load "smartparens" '(diminish 'smartparens-mode))
(eval-after-load "eldoc" '(diminish 'eldoc-mode))

;; Remove the *Compile-Log* buffer if it's empty
(add-hook 'emacs-startup-hook
  (lambda ()
    (let ((compile-log-buffer (get-buffer "*Compile-Log*")))
      (when (and compile-log-buffer
                 (= (buffer-size compile-log-buffer) 0))
                (kill-buffer compile-log-buffer)))))
