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
(setq recentf-exclude
      (append recentf-exclude
              '("ido.last")))
(recentf-mode 1)


;; ido-mode allows you to more easily navigate choices. For example,
;; when you want to switch buffers, ido presents you with a list
;; of buffers in the the mini-buffer. As you start to type a buffer's
;; name, ido will narrow down the list of buffers to match the text
;; you've typed in
;; http://www.emacswiki.org/emacs/InteractivelyDoThings
(ido-mode t)

;; This allows partial matches, e.g. "tl" will match "Tyrion Lannister"
(setq ido-enable-flex-matching t)

;; Turn this behavior off because it's annoying
(setq ido-use-filename-at-point nil)

;; Don't try to match file across all "work" directories; only match files
;; in the current directory displayed in the minibuffer
(setq ido-auto-merge-work-directories-length -1)

;; Do not show virtual buffers
(setq ido-use-virtual-buffers nil)

;; This enables ido in all contexts where it could be useful, not just
;; for selecting buffer and file names
(ido-ubiquitous-mode 1)

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

;; Do not automatically cd to file's directory
;; Copied from https://stackoverflow.com/questions/2626963/how-to-make-emacs-stay-in-the-current-directory.
(defmacro disallow-cd-in-function (fun)
  "Prevent FUN (or any function that FUN calls) from changing directory."
  `(defadvice ,fun (around dissallow-cd activate)
     (let ((old-dir default-directory) ; Save old directory
           (new-buf ad-do-it)) ; Capture new buffer
       ;; If FUN returns a buffer, operate in that buffer in addition
       ;; to current one.
       (when (bufferp new-buf)
         (set-buffer new-buf)
         (setq default-directory old-dir))
       ;; Set default-directory in the current buffer
       (setq default-directory old-dir))))
(disallow-cd-in-function find-file-noselect-1)
(disallow-cd-in-function set-visited-file-name)

;; Use fd instead of 'find' for find-file-in-project
(setq ffip-use-rust-fd t)

;; Use ido-vertical
(require 'ido-vertical-mode)
(ido-vertical-mode 1)
(setq ido-use-faces t)
(setq ido-vertical-show-count t)
(setq ido-vertical-define-keys 'C-n-C-p-up-down-left-right)
(set-face-attribute 'ido-vertical-first-match-face nil
                    :background "#444444"
                    :underline nil)
(set-face-attribute 'ido-vertical-only-match-face nil
                    :background "#444444"
                    :foreground "white"
                    :underline nil)
(set-face-attribute 'ido-vertical-match-face nil
                    :foreground "#e37969"
                    :underline nil)
