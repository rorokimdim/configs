(require 'epa-file)
(epa-file-enable)

(require 'aes)
(aes-enable-auto-decryption)

;; Encrypt .enc.org files on write
(add-hook
 'before-save-hook
 (lambda ()
   (interactive)
   (when (string-suffix-p ".enc.org" (buffer-file-name))
     (call-interactively 'aes-toggle-encryption))))
