(require 'cl)
(require 'package)

;; Always load newest byte code
(setq load-prefer-newer t)

;; Warn when opening files bigger than 100MB
(setq large-file-warning-threshold 100000000)

;;
;; Tweak garbage collection threshold
;;
(defconst initial-gc-cons-threshold gc-cons-threshold)
(setq gc-cons-threshold (* 128 1024 1024))
(defun my-minibuffer-setup-hook ()
    (setq gc-cons-threshold most-positive-fixnum))

(defun my-minibuffer-exit-hook ()
    (setq gc-cons-threshold initial-gc-cons-threshold))

(add-hook 'minibuffer-setup-hook #'my-minibuffer-setup-hook)
(add-hook 'minibuffer-exit-hook #'my-minibuffer-exit-hook)
(add-hook 'after-init-hook (lambda () (setq gc-cons-threshold initial-gc-cons-threshold)))

;; Define the following variables to remove the compile-log warnings
(defvar apropos-do-all nil)
(defvar ido-auto-merge-work-directories-length nil)
(defvar ido-cur-item nil)
(defvar ido-cur-list nil)
(defvar ido-default-item nil)
(defvar ido-enable-flex-matching nil)
(defvar ido-use-filename-at-point nil)
(defvar ido-use-virtual-buffers nil)
(defvar inherit-input-method nil)
(defvar predicate nil)
(defvar smex-save-file nil)

;; Silence ad-redefinition warning
(setq ad-redefinition-action 'accept)

(setq diary-file "~/workspace/org/diary")

;; Confirm before killing emacs
(setq confirm-kill-emacs 'y-or-n-p)

;; Config changes made through the customize UI will be stored here
(setq custom-file (expand-file-name "custom.el" "~/.emacs.d"))

;; Things to do after emacs finishing loading
(add-hook 'emacs-startup-hook
          (lambda ()
            (cd "~/workspace")
            (eyebrowse-switch-to-window-config-1)
            (switch-to-buffer "*scratch*")
            (eyebrowse-switch-to-window-config-0)
            (switch-to-buffer "*scratch*")))

;;
;; Packages
;;

;; Define package repositories
(add-to-list 'package-archives '("marmalade" . "http://marmalade-repo.org/packages/") t)
(add-to-list 'package-archives '("melpa" . "http://melpa.milkbox.net/packages/") t)
(add-to-list 'package-archives '("org" . "http://orgmode.org/elpa/") t)

(defvar my-packages
  '(
    avy
    ace-link
    ace-window
    aes
    ag
    aggressive-indent
    anzu

    jbeans-theme

    clojure-mode
    clojure-mode-extra-font-locking
    clojure-snippets
    cider

    company
    company-math
    company-restclient

    ;; elisp libraries
    a
    dash
    f
    ht
    s
    ts

    datomic-snippets
    diminish

    docker-compose-mode

    elpy
    eshell-autojump
    eshell-up

    elm-mode

    evil
    evil-anzu
    evil-escape
    evil-leader
    evil-magit
    evil-numbers
    evil-surround
    evil-visualstar

    emamux

    bind-map
    expand-region
    eyebrowse
    find-file-in-project
    geiser
    gnuplot
    google-maps
    google-this

    helm
    helm-describe-modes
    helm-descbinds
    helm-swoop
    helm-tramp

    highlight-parentheses
    hl-sexp
    rainbow-blocks
    rainbow-delimiters
    smartparens
    paxedit

    dumb-jump

    htmlize
    ido-completing-read+
    ido-vertical-mode
    json-mode
    jq-mode  ;; install jq command first
    magit
    move-text
    multiple-cursors
    ranger

    org
    org-bullets
    origami
    ox-rst

    org-re-reveal
    popwin

    realgud
    restart-emacs
    restclient
    rotate

    smex
    sqlup-mode
    synonymous
    tagedit
    use-package

    virtualenvwrapper
    visual-regexp
    visual-regexp-steroids
    which-key
    world-time-mode
    yasnippet

    dash-at-point

    lsp-mode
    lsp-ui
    company-lsp

    rust-mode
    racer
    cargo

    lua-mode

    flycheck

    company-ghci
    flymake-haskell-multi
    haskell-mode

    ccls

    ess

    julia-mode)
  "A list of packages to ensure are installed at launch.")

(defun my-require-package (package)
  "Installs PACKAGE unless already installed."
  (unless (memq package my-packages)
    (add-to-list 'my-packages package))
  (unless (package-installed-p package)
    (package-install package)))

(defun my-require-packages (packages)
  "Ensures PACKAGES are installed. Missing packages are installed automatically."
  (mapc #'my-require-package packages))

(defun my-packages-installed-p ()
  "Checks if all packages in my-packages are installed."
  (every #'package-installed-p my-packages))

(defun my-install-packages ()
  "Installs all packages in my-packages."
  (unless (my-packages-installed-p)
    (message "%s" "Refreshing package database...")
    (package-refresh-contents)
    (message "%s" " done.")
    (my-require-packages my-packages)))

(defun my-list-foreign-packages ()
  "Browse installed packages not in my-packages."
  (interactive)
  (package-show-package-list
   (set-difference package-activated-list my-packages)))

(package-initialize)
(my-install-packages)

(eval-when-compile
  (require 'use-package))


;;
;; Load custom scripts
;;

(defvar core-dir "~/.emacs.d/core")
(defvar customizations-dir "~/.emacs.d/customizations")
(byte-recompile-directory core-dir 0)
(byte-recompile-directory customizations-dir 0)

;; Load all scripts in core directory in lexicographic order
(mapc 'load (directory-files core-dir 't "^[^#\.].*elc$"))

;; Load all scripts in customizations-dir in lexicographic order
(mapc 'load (directory-files customizations-dir 't "^[^#\.].*elc$"))


;;
;; End of config.
;;
