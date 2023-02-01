;; Disable auto subscript/superscript when using underscores and ^.
;; If needed, we can use a_{2} or a^{2}
(setq org-export-with-sub-superscripts '{})

;; Enable ox-rst
(require 'ox-rst)

;; For <s Tab shortcut to insert source block
(require 'org-tempo)

;; Enable org-babel languages
(org-babel-do-load-languages
 'org-babel-load-languages
 '(
   (clojure . t)
   (dot . t)
   (emacs-lisp . t)
   (gnuplot . t)
   (python . t)
   (racket . t)
   (restclient . t)
   (R . t)
   ))

;; Don't ask to evaluate certain languages
(defun my/org-confirm-babel-evaluate (lang body)
  (not (or (string= lang "clojure")
           (string= lang "dot")
           (string= lang "elisp")
           (string= lang "emacs-lisp")
           (string= lang "gnuplot")
           (string= lang "python")
           (string= lang "racket")
           (string= lang "restclient")
           (string= lang "R")
           )))
(setq org-confirm-babel-evaluate 'my/org-confirm-babel-evaluate)

;; Do not indent after using org-edit-special
(setq org-edit-src-content-indentation 0
      org-src-tab-acts-natively t
      org-src-preserve-indentation t)

;; Use latexmk with xelatex
(setq org-latex-pdf-process '("latexmk --shell-escape -pdf -xelatex %f"))

;; Use minted for source-code highlighting
;; Requires minted packages: sudo tlmgr install minted
(setq org-latex-listings 'minted
      org-latex-packages-alist '(("" "minted"))
      org-latex-minted-options '(("frame" "single")
                                 ("framesep" "6pt")
                                 ("mathescape" "true")))

;; For exporting to reveal-js presentations
(require 'org-re-reveal)

;; Fix source code coloring in html exports
(defun my/org-inline-css-hook (exporter)
  "Insert custom inline css to automatically set the
 background of code to whatever theme I'm using's background"
  (when (eq exporter 'html)
    (let* ((my/pre-bg (face-background 'default))
           (my/pre-fg (face-foreground 'default)))
      (setq
       org-html-head-extra
       (concat
        org-html-head-extra
        (format
         "<style type=\"text/css\">\n pre.src {background-color: %s; color: %s;}
          pre.src:before {background-color: %s; color: %s; top: 10px;}</style>\n"
         my/pre-bg my/pre-fg my/pre-bg my/pre-fg))))))
(add-hook 'org-export-before-processing-hook #'my/org-inline-css-hook)

(defun my/ob-clear-all-results ()
  "Clear all results in the buffer."
  (interactive)
  (save-excursion
    (goto-char (point-min))
    (while (org-babel-next-src-block)
      (org-babel-remove-result))))

;; Shortcuts for org mode
(require 'bind-map)
(bind-map my/org-mode-map
  :keys ("s-,")
  :evil-keys (",")
  :evil-states (normal visual)
  :major-modes (org-mode)
  :bindings ("nd" 'narrow-to-defun
             "ne" 'org-narrow-to-element
             "ns" 'org-narrow-to-subtree
             "nw" 'widen
             "t" 'org-babel-tangle
             "el" 'org-latex-export-to-latex
             "eh" 'org-html-export-to-html
             "l" 'my/ob-clear-all-results))
