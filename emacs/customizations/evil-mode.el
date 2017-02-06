(require 'evil)
(evil-mode t)

(require 'evil-leader)
(global-evil-leader-mode)
(evil-leader/set-leader ",")
(evil-leader/set-key
    "b" 'switch-to-buffer
    "w" 'save-buffer)

(require 'evil-surround)
(global-evil-surround-mode 1)
