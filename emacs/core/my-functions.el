;; require all useful elisp libraries even if not used in this file,
;; so they are available when I need them.
(require 'ag)
(require 'cl)
(require 'dash)
(require 'emamux)
(require 'f)
(require 'ht)
(require 's)

(defun my-eval-and-replace ()
  "Replace preceding sexp with its value."
  (interactive)
  (forward-char)
  (backward-kill-sexp)
  (condition-case nil
      (prin1 (eval (read (current-kill 0)))
             (current-buffer))
    (error (message "Invalid expression")
           (insert (current-kill 0)))))

(defun my-eval-print-last-sexp ()
  "Make eval-print-last-sexp work in evil mode."
  (interactive)
  (forward-char)
  (eval-print-last-sexp))

(defun my-kill-this-buffer-if-unique-else-delete-window ()
  "Kills current buffer if buffer is different from buffers in other visible windows,
   else just deletes the active window."
  (interactive)
  (let* ((bs (mapcar #'window-buffer (window-list)))
         (matches (remove-if-not (lambda (b) (equal b (current-buffer))) bs)))
    (if (= (length matches) 1)
        (if (display-graphic-p) (kill-this-buffer) (evil-quit))
      (delete-window))))

(defvar my-uninteresting-buffer-prefixes
  '("*Messages*"
    "*Compile-Log*"
    "*magit"
    "*nrepl-server"
    "*racket-command-output*"
    "*scratch*"
    "*helm"
    "*Helm"
    "*Backtrace*"
    "*Completions*"
    "*Geiser dbg*"
    " *"))

(defun my-uninteresting-buffer? (buffer-name)
  "Checks if BUFFER-NAME is uninteresting."
  (-any? (lambda (x) (string-prefix-p x buffer-name)) my-uninteresting-buffer-prefixes))

(defun my-interesting-buffer-names ()
  "Gets names of interesting buffers that are currently open."
  (-remove 'my-uninteresting-buffer? (mapcar (function buffer-name) (buffer-list))))

(defun my-shell-command-to-buffer ()
  "Runs a shell command and appends output to current buffer starting from current point."
  (interactive)
  (with-current-buffer (current-buffer)
    (insert (shell-command-to-string (read-string "Enter command: ")))))

(defun my-buffer-toggle (switcher)
  "Toggles buffer using SWITCHER function, skipping over any useless buffers."
  (interactive)
  (if (and (or (string-prefix-p "*terminal" (buffer-name))
               (string-prefix-p "*eshell" (buffer-name)))
           (= 1 (length (my-interesting-buffer-names))))
      (switch-to-buffer "*scratch*")
    (when (my-interesting-buffer-names)
      (funcall switcher)
      (when (my-uninteresting-buffer? (buffer-name))
        (my-buffer-toggle switcher))))

  (when (string-prefix-p "*terminal" (buffer-name))
    (call-interactively 'end-of-buffer)))

(defun my-add-pretty-symbols ()
  "Sets my pretty-symbol mappings."
  (setq prettify-symbols-alist
        '(("alpha" . 945)
          ("beta" . 946)
          ("gamma" . 947)
          ("delta" . 948)
          ("epsilon" . 949)
          ("zeta" . 950)
          ("eta" . 951)
          ("theta" . 952)
          ("lambda" . 955)
          ("mu" . 956)
          ("pi" . 960)
          ("phi" . 966)
          ("psi" . 968)
          ("omega" . 969))))

(defun my-python-add-breakpoint ()
  "Adds ipbp trace."
  (interactive)
  (call-interactively 'evil-append-line)
  (newline-and-indent)
  (insert "import ipdb; ipdb.set_trace()")
  (highlight-lines-matching-regexp "^[ ]*import ipdb; ipdb.set_trace()"))

(defvar my-workspace-directory
  "~/workspace/"
  "Path to my workspace directory")

(defun my-format-date (format)
  "Inserts date in FORMAT format."
  (let ((system-time-locale "en_US.UTF-8"))
    (insert (format-time-string format))))

(defun my-insert-date-and-time ()
  "Inserts current date and time."
  (interactive)
  (my-format-date "[%Y-%m-%d %H:%M:%S [%A, %B]]"))

(defun my-kill-all-buffers ()
  "Kills all buffers except *scratch*."
  (interactive)
  (mapc 'kill-buffer (delq "*scratch*" (buffer-list))))

(defun my-browse-project-directory ()
  "Opens deer on project root directory."
  (interactive)
  (deer (ffip-get-project-root-directory)))

(defun my-split-window-vertically ()
  "Splits window vertically."
  (interactive)
  (call-interactively 'split-window-below)
  (other-window 1 nil)
  (switch-to-buffer "*scratch*"))

(defun my-split-window-horizontally ()
  "Splits window horizontally."
  (interactive)
  (call-interactively 'split-window-right)
  (other-window 1 nil)
  (switch-to-buffer "*scratch*"))

(defun my-tmux-cd-workspace ()
  "Opens a new tmux window on a workspace directory."
  (interactive)
  (emamux:tmux-run-command
   nil
   "new-window"
   "cd ~/workspace/$(ls -d ~/workspace/*/ | sed \"s/.*workspace//\" | sed \"s:/::g\" | fzf); exec bash"))

(defun my-tmux-new-editor ()
  "Opens new instance of editor in a new tmux window."
  (interactive)
  (emamux:tmux-run-command nil "new-window" "emacsclient" "-t"))

(defun my-tmux-open-emacs-config ()
  "Opens a config file in a new tmux window."
  (interactive)
  (emamux:tmux-run-command nil "new-window" "cd ~/workspace/configs/emacs/; emacsclient -t $(fd -e el | fzf)"))

(defun my-tmux-open-shell-in-buffer-directory ()
  "Opens a shell a small tmux window at the top.
   The shell is opened in current buffer's directory. If no buffer is open, opens shell in ~/workspace."
  (interactive)
  (emamux:tmux-run-command
   nil
   "split-window"
   "-vb" "-p 20"
   (concat "cd "
           (if buffer-file-name (file-name-directory buffer-file-name) "~/workspace")
           "; exec bash")))

(defun my-tmux-open-shell-in-project-directory ()
  "Opens a shell a small tmux window at the top.
   The shell is opened in project directory."
  (interactive)
  (emamux:tmux-run-command
   nil
   "split-window"
   "-vb" "-p 20"
   (concat "cd "
           (ffip-get-project-root-directory)
           "; exec bash")))


(defun my-eshell-ag (string)
  "Searches for STRING in the current eshell directory using ag command."
  (interactive)
  (ag/search string (eshell/pwd))
  (delete-window))

(defun my-search-with-ag ()
  "Searches with ag interactively."
  (interactive)
  (call-interactively 'ag)
  (delete-window))

(defun my-goto-function (x)
  "Goes to defintion of a function with name X."
  (find-function (intern x)))

(defun my-cider-figwheel-repl ()
  "Starts a figwheel repl."
  (interactive)
  (save-some-buffers)
  (with-current-buffer (cider-current-repl-buffer)
    (goto-char (point-max))
    (insert "(require 'figwheel-sidecar.repl-api)
             (figwheel-sidecar.repl-api/start-figwheel!) ; idempotent
             (figwheel-sidecar.repl-api/cljs-repl)")
    (cider-repl-return)))

(defun my-get-closest-pathname (file max-level)
  "Determine the pathname of the first instance of FILE starting from the current directory towards
   root. This may not do the correct thing in presence of links. If it does not find FILE, then it
   shall return the name of FILE in the current directory, suitable for creation.

   Copied from: https://www.emacswiki.org/emacs/CompileCommand"
  (let ((root (expand-file-name "/"))
        (level 0))
    (expand-file-name
     file
     (loop
      for d = default-directory then (expand-file-name ".." d)
      do (setq level (+ level 1))
      if (file-exists-p (expand-file-name file d)) return d
      if (equal d root) return nil))))

;;
;; Key bindings
;;
(bind-keys
 :map global-map
 ("C-c C-t" . my-insert-date-and-time))