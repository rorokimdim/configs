(require '[cemerick.pomegranate])

(cemerick.pomegranate/add-dependencies
 :coordinates '[[org.clojure/math.combinatorics "0.1.6"]
                [org.clojure/tools.cli "0.4.2"]
                [cheshire "5.9.0"]
                [table "0.5.0"]
                [rhizome "0.2.9"]
                [clj-http "3.10.0"]
                [clj-time "0.14.0"]
                [com.rpl/specter "1.1.2"]]
 :repositories (merge cemerick.pomegranate.aether/maven-central
                      {"clojars" "https://clojars.org/repo"}))

(require '[clojure.tools.cli :refer [parse-opts]]
         '[clojure.math.combinatorics :as c]
         '[cheshire.core :as j]
         '[table.core :refer [table table-str]]
         '[clj-http.client :as http]
         '[clj-time.core :as t]
         '[rhizome.viz :as viz]
         '[com.rpl.specter :as s])

;; From https://github.com/mikera/clojure-utils/blob/master/src/main/clojure/mikera/cljutils/namespace.clj#L165
(defmacro with-ns
  "Evaluates body in another namespace. ns is either a namespace
  object or a symbol. This makes it possible to define functions in
  namespaces other than the current one."
  [ns & body]
  `(binding [*ns* (the-ns '~ns)]
     ~@(map (fn [form] `(eval '~form)) body)))

;; From https://github.com/mikera/clojure-utils/blob/master/src/main/clojure/mikera/cljutils/namespace.clj#L173
(defmacro with-temp-ns
  "Evaluates body in an anonymous namespace, which is then immediately
  removed. The temporary namespace will 'refer' clojure.core."
  [& body]
  `(try
     (create-ns 'sym#)
     (let [result# (with-ns sym#
                     (clojure.core/refer-clojure)
                     ~@body)]
       result#)
     (finally (remove-ns 'sym#))))

(defn start-nrepl
  "Starts nrepl."
  ([] (start-nrepl 7888))
  ([port]
   (eval
    `(do
       (require '[cemerick.pomegranate])
       (cemerick.pomegranate/add-dependencies
        :coordinates '[[org.clojure/tools.nrepl "0.2.13"]]
        :repositories (merge cemerick.pomegranate.aether/maven-central
                             {"clojars" "https://clojars.org/repo"}))
       (require '[clojure.tools.nrepl.server])
       (println "Starting nrepl at" ~port)
       (defonce server (clojure.tools.nrepl.server/start-server :port ~port))))))

(defcmd closh [path & args]
  "Runs a file at given `path`. Expects a function named `main` that accepts given `args`."
  (let [sargs (map str args)
        main-call (conj sargs 'main)]
    (eval (macroexpand
           `(with-temp-ns
              (eval closh.zero.env/*closh-environment-requires*)
              (load-string (slurp ~path))
              (eval ~main-call))))))

(defcmd e [& args]
  "Opens a file using emacsclient."
  (eval
   (macroexpand
    `(sh "emacsclient"
         "-t"
         "--alternate-editor"
         "\"\""
         ~@args))))

(defcmd r [& args]
  "Alias for ranger command."
  (eval (macroexpand `(sh "ranger" ~@args))))

(defcmd git-delete-merged-branches
  "Deletes local git branches that are merged in remote master."
  []
  (let [branches-to-exclude '(develop master stage)
        grep-exclude-pattern (str/join "\\|" branches-to-exclude)]
    (sh git branch --merged origin/master | grep -v (identity grep-exclude-pattern) | xargs -n1 git branch -d)))

(defcmd find-files
  "Gets list of files matching a glob-pattern."
  ([glob-pattern] (find-files "." glob-pattern))
  ([path glob-pattern]
   (eval (macroexpand
          `(sh-lines "find" ~path "-name" ~glob-pattern)))))

(defcmd slurp-json
  "Slurps json data from a uri."
  [uri]
    (-> uri
      slurp
      (j/decode true)))
