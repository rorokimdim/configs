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

;;
;; Makes clipboard work in terminal sessions.
;;

;; Sometimes I want clipboard even on non-display terminals, but
;; that could be slow depending on how the clipboard is accessed. By
;; default don't allow it, other than in certain modes (like restclient-mode).
(setq my-allow-non-display-clipboard-default nil)
(defun my-toggle-non-display-clipboard ()
  "Toggles if clipboard copying should be allowed in non-display environemnt by default."
  (setq my-allow-non-display-clipboard-default
        (not my-allow-non-display-clipboard-default)))
(defun my-allow-non-display-clipboard? ()
  "Checks if clipboard copying should be allowed in non-display environemnt."
  (or my-allow-non-display-clipboard-default
      (string= 'restclient-mode major-mode)))
(setq my-copy-command (cond ((eq system-type 'darwin) "pbcopy")
                            ((or (eq system-type 'gnu/linux) (eq system-type 'linux)) "xsel -ib")))

(setq my-paste-command (cond ((eq system-type 'darwin) "pbpaste")
                             ((or (eq system-type 'gnu/linux) (eq system-type 'linux)) "xsel -ob")))

(unless window-system
  (defun cut-function (text &optional push)
    (when (or (my-allow-non-display-clipboard?) (getenv "DISPLAY"))
      (with-temp-buffer
        (insert text)
        (call-process-region (point-min) (point-max) my-copy-command))))
  (setq interprogram-cut-function 'cut-function))

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
 '(helm-selection ((t (:background "black" :foreground "gray100"))))
 '(helm-source-header ((t (:weight bold :height 1.3 :family "Sans Serif"))))
 '(highlight-indentation-face ((t (:inherit nil))))
 '(hl-line ((t (:background "Black"))))
 '(hl-paren-face ((t (:weight ultra-bold))) t)
 '(sp-show-pair-enclosing ((t (:underline t))))
 '(sp-show-pair-match-face ((t (:underline t))))
 '(sp-show-pair-mismatch-face ((t (:strike-through t))))
 '(rainbow-delimiters-depth-1-face ((t (:foreground "#78c5d6"))))
 '(rainbow-delimiters-depth-2-face ((t (:foreground "#bf62a6"))))
 '(rainbow-delimiters-depth-3-face ((t (:foreground "#459ba8"))))
 '(rainbow-delimiters-depth-4-face ((t (:foreground "#e868a2"))))
 '(rainbow-delimiters-depth-5-face ((t (:foreground "#79c267"))))
 '(rainbow-delimiters-depth-6-face ((t (:foreground "#f28c33"))))
 '(rainbow-delimiters-depth-7-face ((t (:foreground "#c5d647"))))
 '(rainbow-delimiters-depth-8-face ((t (:foreground "#f5d63d"))))
 '(rainbow-delimiters-depth-9-face ((t (:foreground "#78c5d6")))))

;; Change neotree width
(custom-set-variables
   '(size-indication-mode t)
   '(hl-paren-colors (quote ("firebrick1")))
   '(sp-show-pair-from-inside t)
   '(neo-window-width 50))

;; Enable mouse support on terminal
(unless window-system
  (require 'mouse)
  (xterm-mouse-mode t))

;; Cleanup my mode-line
(eval-after-load "undo-tree" '(diminish 'undo-tree-mode))
(eval-after-load "smartparens" '(diminish 'smartparens-mode))
(eval-after-load "eldoc" '(diminish 'eldoc-mode))
(eval-after-load "anzu" '(diminish 'anzu-mode))
(eval-after-load "magit" '(diminish 'auto-revert-mode))
(eval-after-load "which-key" '(diminish 'which-key-mode))
(eval-after-load "vi-tilde-fringe" '(diminish 'vi-tilde-fringe-mode))
(eval-after-load "highlight-parentheses" '(diminish 'highlight-parentheses-mode))

;; Remove the *Compile-Log* buffer if it's empty
(add-hook 'emacs-startup-hook
          (lambda ()
            (let ((compile-log-buffer (get-buffer "*Compile-Log*")))
              (when (and compile-log-buffer
                         (= (buffer-size compile-log-buffer) 0))
                (kill-buffer compile-log-buffer)))))

;;
;; Configure term
;;

;; Kill buffer on exit
(defadvice term-handle-exit
    (after term-kill-buffer-on-exit activate)
  (kill-buffer))
;; Default to bash
(defvar my-term-shell "/bin/bash")
(defadvice term (before force-bash)
  (interactive (list my-term-shell)))
(ad-activate 'term)
;; Use C-x as command prefix
(add-hook 'term-mode-hook
          '(lambda ()
             (term-set-escape-char ?\C-x)))

;; Ag
(setq ag-highlight-search t)
(setq ag-reuse-window 't)

;; Anzu mode
(global-anzu-mode +1)

;; Setup which-key mode
(require 'which-key)
(which-key-mode)
(which-key-setup-minibuffer)

;; Enable vi-tilde-fringe-mode for all programming modes
(add-hook 'prog-mode-hook 'vi-tilde-fringe-mode)

;; Enable hl-line mode
(global-hl-line-mode 1)
