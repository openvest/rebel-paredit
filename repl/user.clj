(ns user
   (:require [clojure.reflect :as reflect :refer [reflect]]
             [cljfmt.core :as fmt]
             [rewrite-clj.zip :as z]
             [rewrite-clj.node :as n]
             [repl-balance.jline-api :as j]
             [repl-balance.clojure.paredit :as pe]
             [rewrite-clj.paredit :as paredit]
             [rewrite-clj.custom-zipper.utils :as rczu]
             [clojure.pprint :refer [pprint]]
             [clojure.string :as str])
   (:import [org.jline.reader.impl DefaultParser]
            [org.jline.keymap KeyMap]
            [org.jline.utils InfoCmp InfoCmp$Capability]
            [org.jline.reader.impl LineReaderImpl BufferImpl]
            [org.jline.terminal Terminal]))

(defmacro def-let [bindings & body]
  (assert (= 0 (mod (count bindings) 2)) "Must have an even number of bindings")
  `(do ~@(->> bindings
              (partition 2)
              (map (fn [[s v]] (list 'def s v))))
       ~@body))


(def ns-stack (atom nil))

(defn ns-push [new-ns]
  (or (find-ns new-ns)  (require new-ns))
  (swap! ns-stack conj (symbol (str *ns*)))
  (in-ns new-ns)
  (use 'clojure.repl))

(defn ns-pop []
  (let [new-ns (first @ns-stack)]
    (swap! ns-stack rest)
    (in-ns new-ns)))

(comment
  ;; how to get imports or require map from another file
  (->> (clojure.edn/read-string (slurp "repl/user.clj"))
       (drop-while (complement coll?))
       (map (juxt first (comp vec rest)))
       (into {})
       :import
       pprint))
