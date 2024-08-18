(ns rebel-cljs-dev.main
  (:require
   [cljs.repl]
   [cljs.repl.nashorn :as nash]
   [repl-balance.cljs.repl :as cljs-repl]
   [repl-balance.cljs.service.local :as cljs-service]
   [repl-balance.clojure.line-reader :as clj-line-reader]
   [repl-balance.core :as rebel]
   [repl-balance.utils :as utils]
   [repl-balance.jline-api :as api]))

(defn -main [& args]
    (let [repl-env (nash/repl-env)
        service (cljs-service/create {:repl-env repl-env})]
      (println "This is the DEVELOPMENT REPL in rebel-cljs-dev.main")
      (println (rebel/help-message))
    (binding [utils/*debug-log* true]
      (if (= "stream" (first args))
        (do
          (println "[[Input stream line reader]]")
          (rebel/with-readline-in
            (clj-line-reader/create service)
            (cljs.repl/repl
             repl-env
             :prompt (fn [])
             :print cljs-repl/syntax-highlight-println)))
        (rebel/with-line-reader
          (clj-line-reader/create service)
          (cljs.repl/repl
           repl-env
           :prompt (fn [])
           :print cljs-repl/syntax-highlight-println
           :read (cljs-repl/create-repl-read)))))))
