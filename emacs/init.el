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
    ag
    aggressive-indent

    cider
    clojure-mode
    clojure-mode-extra-font-locking
    clojure-snippets

    diminish

    elpy

    exec-path-from-shell

    evil
    evil-leader
    evil-surround

    geiser
    highlight-sexp
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
    yasnippet))

(dolist (p my-packages)
  (when (not (package-installed-p p))
    (package-install p)))


;;;;
;; Customization
;;;;
(add-to-list 'load-path "~/.emacs.d/customizations")

;; Loads my custom functions
(load "my-functions.el")

;; Sets up exec-path-from-shell so that Emacs will use the correct
;; environment variables
(load "shell-integration.el")

;; These customizations make it easier for you to navigate files,
;; switch buffers, and choose options from the minibuffer.
(load "navigation.el")

;; These customizations make editing a bit nicer.
(load "editing.el")

;; Hard-to-categorize customizations
(load "misc.el")

;; For editing lisps
(load "lisp-editing.el")

;; Language-specific setups
(load "setup-clojure.el")
(load "setup-python.el")

;; Keyboard shortcuts
(load "keys.el")

;; Vim mode
(load "evil-mode.el")

;; These customizations change the way emacs looks and disable/enable
;; some user interface elements
(load "ui.el")

;; Load any custom configs that is specific to the
;; machine I'm using.
(when (file-exists-p "~/.emacs.d/customizations/machine-custom.el")
  (load "machine-custom.el"))
;;
;; End of config.
;; Feel free to delete the rest if emacs adds it.
;;
