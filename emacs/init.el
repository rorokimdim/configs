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

;;;;
;; Packages
;;;;

;; Define package repositories
(require 'package)
(add-to-list 'package-archives
             '("marmalade" . "http://marmalade-repo.org/packages/") t)
(add-to-list 'package-archives
             '("tromey" . "http://tromey.com/elpa/") t)
(add-to-list 'package-archives
             '("melpa" . "http://melpa.milkbox.net/packages/") t)
(add-to-list 'package-archives
             '("org" . "http://orgmode.org/elpa/") t)

;; Load and activate emacs packages. Do this first so that the
;; packages are loaded before you start trying to modify them.
;; This also sets the load path.
(package-initialize)

;; Download the ELPA archive description if needed.
;; This informs Emacs about the latest versions of all packages, and
;; makes them available for download.
(when (not package-archive-contents)
  (package-refresh-contents))


;; Define he following variables to remove the compile-log warnings
;; when defining ido-ubiquitous
(defvar ido-cur-item nil)
(defvar ido-default-item nil)
(defvar ido-cur-list nil)
(defvar predicate nil)
(defvar inherit-input-method nil)

;; Packages to install
(defvar my-packages
  '(
    ace-jump-mode
    ag
    aggressive-indent

    cider
    clojure-mode
    clojure-mode-extra-font-locking
    clojure-snippets

    diminish
    elpy
    expand-region

    evil
    evil-leader
    evil-surround

    geiser
    ido-ubiquitous
    magit
    multi-term
    multiple-cursors
    neotree
    org
    persp-projectile
    perspective
    projectile
    rainbow-delimiters
    restart-emacs
    restclient
    smartparens
    smex
    tagedit
    yasnippet

    use-package))

(dolist (p my-packages)
  (when (not (package-installed-p p))
    (package-install p)))

(eval-when-compile
  (require 'use-package))

;;;;
;; Customization
;;;;
(byte-recompile-directory "~/.emacs.d/customizations" 0)
(add-to-list 'load-path "~/.emacs.d/customizations")

;; Loads my custom functions
(load "my-functions.elc")

;; These customizations make it easier for you to navigate files,
;; switch buffers, and choose options from the minibuffer.
(load "navigation.elc")

;; These customizations make editing a bit nicer.
(load "editing.elc")

;; Hard-to-categorize customizations
(load "misc.elc")

;; For editing lisps
(load "lisp-editing.elc")

;; Language-specific setups
(load "setup-clojure.elc")
(load "setup-python.elc")

;; Keyboard shortcuts
(load "keys.elc")

;; Vim mode
(load "evil-mode.elc")

;; UI customizations
(load "ui.elc")

;; Load any custom configs that is specific to the
;; machine I'm using.
(when (file-exists-p "~/.emacs.d/customizations/machine-custom.elc")
  (load "machine-custom.elc"))
;;
;; End of config.
;; Feel free to delete the rest if emacs adds it.
;;
