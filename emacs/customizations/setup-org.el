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
