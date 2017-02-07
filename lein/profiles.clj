{
  :user {
    :plugins [
      [lein-gorilla "0.3.6"]
      [cider/cider-nrepl "0.15.0-SNAPSHOT"]
      [luminus/lein-template "2.9.10.74"]
    ]
    :dependencies [
      [lein-cljfmt "0.5.6"]
      [org.clojure/tools.trace "0.7.9"]
      [jonase/eastwood "0.2.1" :exclusions [org.clojure/clojure]]
    ]
  }
}
