;; Disable auto subscript/superscript when using underscores and ^.
;; If needed, we can use a_{2} or a^{2}
(setq org-export-with-sub-superscripts '{})

;; Enable org-bullets
(require 'org-bullets)
(add-hook 'org-mode-hook (lambda () (org-bullets-mode 1)))

;; Enable org-babel languages
(org-babel-do-load-languages
 'org-babel-load-languages
 '((emacs-lisp . t)
   (python . t)
   (gnuplot . t)
   (clojure . t)))

;; Use xelatex
;; Run it multipe times to get toc/page-numbering/references
(setq org-latex-pdf-process '("xelatex -shell-escape %f"
                              "xelatex -shell-escape %f"
                              "xelatex -shell-escape %f"))

;; Use minted for source-code highlighting
;; Requires minted packages: sudo tlmgr install minted
(setq org-latex-listings 'minted
      org-latex-packages-alist '(("" "minted")))

;; For exporting to reveal-js presentations
(require 'ox-reveal)

;; Fix source code coloring in html exports
(defun my-org-inline-css-hook (exporter)
  "Insert custom inline css to automatically set the
 background of code to whatever theme I'm using's background"
  (when (eq exporter 'html)
    (let* ((my-pre-bg (face-background 'default))
           (my-pre-fg (face-foreground 'default)))
      (setq
       org-html-head-extra
       (concat
        org-html-head-extra
        (format
         "<style type=\"text/css\">\n pre.src {background-color: %s; color: %s;}
          pre.src:before {background-color: %s; color: %s; top: 10px;}</style>\n"
         my-pre-bg my-pre-fg my-pre-bg my-pre-fg))))))
(add-hook 'org-export-before-processing-hook #'my-org-inline-css-hook)
