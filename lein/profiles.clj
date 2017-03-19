{
  :user {
    :plugins [
      [lein-gorilla "0.4.0"]
      [cider/cider-nrepl "0.15.0-SNAPSHOT"]
    ]
    :dependencies [
      [lein-cljfmt "0.5.6"]
      [org.clojure/tools.trace "0.7.9"]
      [jonase/eastwood "0.2.3" :exclusions [org.clojure/clojure]]
    ]
  }
}
