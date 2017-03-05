;; Saner background
(setq shr-color-visible-luminance-min 80)

;; Limit content width to 80 columns
(setq shr-width 80)

;; Shortcuts for eww mode
(require 'evil-leader)
(evil-leader/set-key-for-mode 'eww-mode
  "a" 'ace-link-eww
  "r" 'eww-reload)
