(defproject com.openvest/repl-balance-cljs "0.1.5-SNAPSHOT"
  :description "A rebel readline service for ClojureScript"
  :url "https://github.com.openvest/repl-balance"
  :license {:name "Eclipse Public License"
            :distribution :repo
            :url "http://www.eclipse.org/legal/epl-v10.html"}
  :scm {:name "git"
        :url "https://github.com.openvest/repl-balance"
        :dir ".."}

  :dependencies [[com.openvest/repl-balance "0.1.5-SNAPSHOT"]
                 [org.clojure/clojurescript "1.9.946"]
                 [cljs-tooling "0.2.0"]]

  :profiles {:dev {:source-paths ["src" "dev"]
                   :main rebel-cljs-dev.main}})
