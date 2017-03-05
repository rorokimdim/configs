;; Use restclient mode for .rest files
(add-to-list 'auto-mode-alist '("\\.rest\\'" . restclient-mode))

;; Shortcuts for restclient mode
(require 'evil-leader)
(evil-leader/set-key-for-mode 'restclient-mode
  "er" 'restclient-http-send-current-raw
  "ef" 'restclient-http-send-current
  "ec" 'restclient-copy-curl-command
  "rn" 'restclient-jump-next
  "rp" 'restclient-jump-prev)
