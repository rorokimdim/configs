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
             '("melpa" . "http://melpa.milkbox.net/packages/") t)

;; Load and activate emacs packages. Do this first so that the
;; packages are loaded before you start trying to modify them.
;; This also sets the load path.
(package-initialize)

;; Download the ELPA archive description if needed.
;; This informs Emacs about the latest versions of all packages, and
;; makes them available for download.
(when (not package-archive-contents)
  (package-refresh-contents))


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

;; Do this before anything 'evil to make 'tab' work in org-mode for org-cycle
(setq evil-want-C-i-jump nil)

;; Packages to install
(defvar my-packages
  '(
    ace-jump-mode
    ace-link
    ace-window
    ag
    aggressive-indent
    anzu

    cider
    clj-refactor
    clojure-mode
    clojure-mode-extra-font-locking
    clojure-snippets
    datomic-snippets

    gnuplot
    key-chord

    dash
    s

    diminish

    elpy

    eshell-autojump
    eshell-up
    expand-region

    evil
    evil-escape
    evil-leader
    evil-magit
    evil-surround
    evil-visualstar
    evil-anzu

    find-file-in-repository
    geiser

    google-this
    google-maps

    helm
    helm-swoop
    eyebrowse

    multi-term
    helm-mt

    highlight-parentheses
    hl-sexp
    ido-ubiquitous
    ido-vertical-mode
    magit
    multiple-cursors
    neotree

    org
    org-bullets

    sicp

    popwin
    rainbow-blocks
    rainbow-delimiters
    restart-emacs
    restclient
    rotate
    smartparens
    smex
    sqlup-mode
    tagedit
    which-key
    world-time-mode
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

;; Vim mode
(load "evil-mode.elc")

;; These customizations make it easier for you to navigate files,
;; switch buffers, and choose options from the minibuffer.
(load "navigation.elc")

;; These customizations make editing a bit nicer.
(load "editing.elc")

;; Hard-to-categorize customizations
(load "misc.elc")

;; For common lispy things
(load "setup-lisps.elc")

;; Neotree setup
(load "setup-neotree.el")

;; Restclient mode setup
(load "setup-restclient.elc")

;; Language-specific setups
(load "setup-scheme.elc")
(load "setup-clojure.elc")
(load "setup-python.elc")
(load "setup-sql.elc")

;; Keyboard shortcuts
(load "keys.elc")

;; Setup encryption/decryption
(load "setup-crypto.elc")

;; UI customizations
(load "ui.elc")

;; Setup term
(load "setup-term.elc")

;; Setup eshell
(load "setup-eshell.elc")

;; Setup org mode
(load "setup-org.elc")

;; Web browsing customizations
(load "web-browsing.elc")

;; Mac OSX specific stuff
(load "setup-osx.elc")

;; Load any custom configs that is specific to the
;; machine I'm using.
(when (file-exists-p "~/.emacs.d/customizations/machine-custom.elc")
  (load "machine-custom.elc"))

(require 'server)
(unless (server-running-p)
  (server-start))

;; Things to do after emacs finishing loading
(add-hook 'emacs-startup-hook
          (lambda ()
            (cd "~/workspace")
            (eyebrowse-switch-to-window-config-0)
            (find-file "~/workspace/org/todo.org")
            (eyebrowse-switch-to-window-config-1)
            (switch-to-buffer "*scratch*")))
;;
;; End of config.
;; Feel free to delete the rest if emacs adds it.
;;
