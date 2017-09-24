(deftask my-repl-setup "My repl setup task for vim and emacs-cider"
  []
  (require 'boot.repl)
  (swap! @(resolve 'boot.repl/*default-dependencies*)
         concat '[
                  ;; For Emacs Cider
                  [org.clojure/tools.nrepl "0.2.12"]
                  [cider/cider-nrepl "0.16.0-SNAPSHOT"]
                  [refactor-nrepl "2.4.0-SNAPSHOT"]

                  ;; For vim
                  [jonase/eastwood "0.2.4" :exclusions [org.clojure/clojure]]
                  [cljfmt "0.5.1"]

                  ])
  (swap! @(resolve 'boot.repl/*default-middleware*)
         concat '[cider.nrepl/cider-middleware
                  refactor-nrepl.middleware/wrap-refactor])
  identity)
