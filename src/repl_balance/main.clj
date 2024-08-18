(ns repl-balance.main
  (:require
   [repl-balance.clojure.main :as main]))

(defn -main [& args]
  (apply main/-main args))
