;; Every time when the neotree window is opened, let it find current file and jump to node
(setq neo-smart-open t)

;; Fix shortcuts for neotree
(require 'evil)
(evil-define-key 'normal neotree-mode-map (kbd "TAB") 'neotree-enter)
(evil-define-key 'normal neotree-mode-map (kbd "SPC") 'neotree-enter)
(evil-define-key 'normal neotree-mode-map (kbd "RET") 'neotree-enter)
(evil-define-key 'normal neotree-mode-map (kbd "c") 'neotree-change-root)
(evil-define-key 'normal neotree-mode-map (kbd "d") 'neotree-dir)
(evil-define-key 'normal neotree-mode-map (kbd "q") 'neotree-hide)
