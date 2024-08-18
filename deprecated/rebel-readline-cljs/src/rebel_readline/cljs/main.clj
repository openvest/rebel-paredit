(ns repl-balance.cljs.main
  (:require
   [cljs.repl.nashorn :as nash]
   [repl-balance.cljs.repl :as cljs-repl]
   [repl-balance.core :as core]))

;; TODO need ot bring this in line with cljs.main
(defn -main [& args]
  (let [repl-env (nash/repl-env)]
    (cljs-repl/repl repl-env)))
