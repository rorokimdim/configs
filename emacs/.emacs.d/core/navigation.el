;; These customizations make it easier for you to navigate files,
;; switch buffers, and choose options from the minibuffer.


;; "When several buffers visit identically-named files,
;; Emacs must give the buffers distinct names. The usual method
;; for making buffer names unique adds ‘<2>’, ‘<3>’, etc. to the end
;; of the buffer names (all but one of them).
;; The forward naming method includes part of the file's directory
;; name at the beginning of the buffer name
;; https://www.gnu.org/software/emacs/manual/html_node/emacs/Uniquify.html
(require 'uniquify)
(setq uniquify-buffer-name-style 'forward)

;; Turn on recent file mode so that you can more easily switch to
;; recently edited files when you first start emacs
(setq recentf-save-file (concat user-emacs-directory ".recentf"))
(require 'recentf)
(setq recentf-max-saved-items 50
      recentf-max-menu-items 40)
(recentf-mode 1)


;; Shows a list of buffers
(global-set-key (kbd "C-x C-b") 'ibuffer)

;; Enhances M-x to allow easier execution of commands. Provides
;; a filterable list of possible commands in the minibuffer
;; http://www.emacswiki.org/emacs/Smex
(setq smex-save-file (concat user-emacs-directory ".smex-items"))
(smex-initialize)
(global-set-key (kbd "M-x") 'smex)

;; Allow dired-find-alternate-file command
(put 'dired-find-alternate-file 'disabled nil)

;; Use same buffer when jumping to parent dir with "^"
(add-hook
 'dired-mode-hook
 (lambda ()
   (define-key dired-mode-map (kbd "^")
     (lambda () (interactive) (find-alternate-file "..")))))

;; Use ranger instead of dired
(ranger-override-dired-mode t)

;; Use fd instead of 'find' for find-file-in-project
(setq ffip-use-rust-fd t)

;; ivy
(use-package counsel
  :after ivy
  :diminish
  :bind (("M-x" . counsel-M-x))
  :config (counsel-mode))

(use-package ivy
  :defer 0.1
  :diminish
  :bind (("C-c C-r" . ivy-resume)
         ("C-x B" . ivy-switch-buffer-other-window))
  :custom
  (ivy-count-format "(%d/%d) ")
  (ivy-use-virtual-buffers nil)
  :config (ivy-mode))

(use-package swiper
  :after ivy
  :diminish
  :bind (("C-s" . swiper)
         ("C-r" . swiper)))
