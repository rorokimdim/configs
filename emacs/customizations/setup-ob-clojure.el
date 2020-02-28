;;
;; From https://acidwords.com/posts/2016-09-21-literate-programming-with-monroe-and-org-mode.html
;;

(require 'org)
(require 'ob-clojure)

(defun org-babel-execute:clojure-with-monroe-hook (text)
  "Hook executed when we receive processed message from Monroe."
  (when (and (> (length text) 0)
             (not (string-match monroe-prompt-regexp text)))
    (setq org-babel-execute:clojure-with-monroe-result text)))

(defun org-babel-execute:clojure-with-monroe (body params)
  "Execute a block of Clojure code with Babel using Monroe as backend REPL.
   Will ask for nREPL server if there are no connections."
  (let ((expanded (org-babel-expand-body:clojure body params))
        org-babel-execute:clojure-with-monroe-result)
    (require 'monroe)
    (add-hook 'comint-output-filter-functions 'org-babel-execute:clojure-with-monroe-hook)

    ;; try to connect if we don't have running REPL buffer
    (when (not (get-buffer monroe-repl-buffer))
      (call-interactively 'monroe)
      ;; wait a tiny amount of time until Emacs processes pending comint messages
      (accept-process-output (get-buffer-process monroe-repl-buffer) 0.5)
      ;; Monroe will create a buffer in place. Quickly restore previous buffer,
      ;; sending Monroe in background.
      (previous-buffer))

    ;; send code for evaluation
    (with-temp-buffer
      (insert expanded)
      (monroe-eval-buffer))

    ;; again, wait until comint process our hook
    (while (progn
             (accept-process-output (get-buffer-process monroe-repl-buffer) 0.5)
             (null org-babel-execute:clojure-with-monroe-result)))
    (remove-hook 'comint-output-filter-functions 'org-babel-execute:clojure-with-monroe-hook)

    (org-babel-result-cond (cdr (assoc :result-params params))
      org-babel-execute:clojure-with-monroe-result
      (condition-case nil (org-babel-script-escape org-babel-execute:clojure-with-monroe-result)
        (error org-babel-execute:clojure-with-monroe-result)))))

(defun org-babel-execute:clojure-advised (oldfn &rest args)
  "Overriden org-babel-execute:clojure."
  (if (eq 'monroe org-babel-clojure-backend)
    (apply 'org-babel-execute:clojure-with-monroe args)
    (apply oldfn args)))

;; do actual replacement
(advice-add #'org-babel-execute:clojure :around #'org-babel-execute:clojure-advised)

(setq org-babel-clojure-backend 'monroe)
