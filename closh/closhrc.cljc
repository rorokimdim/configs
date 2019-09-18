(require '[clojure.string :as str])

#?(:cljs (require '[cljs.nodejs]
                  '[lumo.io :refer [slurp spit]]))
                                        ;
#?(:cljs (refer-clojure :exclude '[load-file]))
#?(:cljs
   (defn load-file [f]
     (lumo.repl/execute-path f {})))

(defonce my-closhrc-state (atom {:initialized false}))

(defn closh-prompt []
  "Sets up my closh prompt."
  (source-shell "bash" "eval \"$(direnv export bash)\"")
  (str (str/replace-first (sh-str pwd) (getenv "HOME") "~") " ($) "))

(defcmd reload-closhrc []
  "Reloads ~/.closhrc file."
  (load-file (str (getenv "HOME") "/.closhrc")))

(defcmd cdw []
  "CDs to workspace directory."
  (cd (str (getenv "HOME") "/" "workspace")))

(defcmd o []
  "Opens a file selected from fzf using vim."
  (sh "vim" (sh fzf | (str/trim))))

(defcmd j []
  "Jumps to a workspace directory."
  (let [prefix (str (getenv "HOME") "/workspace/")]
    (cd (str prefix
             (sh-str "ls" (identity prefix) | fzf)))))

(defn my-closhrc-run-once []
  "Put all stuff that should only be run once here."
  (when (not (:initialized @my-closhrc-state))
    (source-shell "export CLICOLOR=1") ;; For colored output from `ls` command
    (source-shell (str "source " (getenv "HOME") "/" ".bashrc"))
    (swap! my-closhrc-state assoc :initialized true)))

(my-closhrc-run-once)
#?(:clj (load-file (str (getenv "HOME") "/.closhrc.clj")))
#?(:cljs (load-file (str (getenv "HOME") "/.closhrc.cljs")))
