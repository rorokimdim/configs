(require 'cl-lib)
(require 'package)

;; Silence warning about using package cl
(setq byte-compile-warnings '(cl-functions))

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
            (switch-to-buffer "*scratch*")))

;;
;; Packages
;;

;; Define package repositories
(add-to-list 'package-archives '("melpa" . "http://melpa.org/packages/") t)

(defvar my-packages
  '(
    ;; elisp libraries
    a
    dash
    f
    ht
    s
    ts

    ;; general lispy packages
    highlight-parentheses
    paxedit
    rainbow-blocks
    rainbow-delimiters
    smartparens

    ;; clojure
    clojure-mode
    clojure-mode-extra-font-locking
    clojure-snippets
    cider
    datomic-snippets

    ;; c++
    ccls

    ;; python
    elpy
    virtualenvwrapper

    ;; scheme
    geiser

    ;; org
    org
    origami
    ox-rst
    org-re-reveal

    ;; haskell
    company-ghci
    flymake-haskell-multi
    haskell-mode

    ;; rust
    rust-mode

    ;; javascript
    js2-mode
    skewer-mode

    ;; go
    go-mode

    ;; themes
    jbeans-theme

    ;; rest
    ace-link
    ace-window
    aes
    ag
    aggressive-indent
    anzu
    avy
    bind-map
    centaur-tabs
    company
    company-math
    counsel
    counsel-projectile
    dash-at-point
    diminish
    docker-compose-mode
    dumb-jump
    emamux
    emmet-mode
    eshell-autojump
    eshell-up
    exec-path-from-shell
    expand-region
    find-file-in-project
    flycheck
    helm
    helm-descbinds
    helm-describe-modes
    helm-swoop
    helm-tramp
    htmlize
    ivy
    jq-mode
    json-mode
    lsp-ui
    magit
    move-text
    multiple-cursors
    neotree
    popwin
    projectile
    ranger
    restart-emacs
    rotate
    smex
    sqlup-mode
    swiper
    synonymous
    tagedit
    use-package
    visual-regexp
    visual-regexp-steroids
    which-key
    world-time-mode
    yasnippet

    ;; evil
    evil
    evil-anzu
    evil-escape
    evil-leader
    evil-magit
    evil-numbers
    evil-surround
    evil-visualstar)
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
  (cl-every #'package-installed-p my-packages))

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

;; Setup PATH for GUIs
(when (memq window-system '(mac ns x))
  (setenv "SHELL" "/bin/zsh")
  (exec-path-from-shell-initialize)
  (exec-path-from-shell-copy-envs '("PATH")))

;;
;; Load custom scripts
;;

;; Hack to enable TAB for org-cycle in evil mode
(setq evil-want-C-i-jump nil)

(defvar core-dir "~/.emacs.d/core")
(defvar customizations-dir "~/.emacs.d/customizations")
(defvar third-party-dir "~/.emacs.d/third-party")
(byte-recompile-directory core-dir 0)
(byte-recompile-directory customizations-dir 0)
(byte-recompile-directory third-party-dir 0)

;; Load all scripts in third-party-dir in lexicographic order
(mapc 'load (directory-files third-party-dir 't "^[^#\.].*elc$"))

;; Load all scripts in core directory in lexicographic order
(mapc 'load (directory-files core-dir 't "^[^#\.].*elc$"))

;; Load all scripts in customizations-dir in lexicographic order
(mapc 'load (directory-files customizations-dir 't "^[^#\.].*elc$"))

;;
;; End of config.
;;
