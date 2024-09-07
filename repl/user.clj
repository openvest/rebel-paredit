(ns user
   (:require [clojure.reflect :as reflect :refer [reflect]]
             [cljfmt.core :as fmt]
             [rewrite-clj.zip :as z]
             [rewrite-clj.node :as n]
             [repl-balance.jline-api :as j]
             [repl-balance.clojure.paredit :as pe]
             [repl-balance.clojure.tokenizer :as tokenizer]
             [repl-balance.clojure.sexp :as sexp]
             [rewrite-clj.paredit :as paredit]
             [rewrite-clj.custom-zipper.utils :as rczu]
             [clojure.pprint :refer [pprint]]
             [clojure.string :as str])
   (:import [org.jline.reader.impl DefaultParser]
            [org.jline.keymap KeyMap]
            [org.jline.utils InfoCmp InfoCmp$Capability]
            [org.jline.reader.impl LineReaderImpl BufferImpl]
            [org.jline.terminal Terminal]
            [org.jline.utils AttributedStringBuilder AttributedString AttributedStyle]))

(defmacro def-let
  "This is the standard let macro with the exception that
  it promotes all of the let binding variables to global scope
  Best for interactive investigation of a let structure
  e.g. simply change a let to let-def and work with the bindings
       in global scope for debugging"
  [bindings & body]
  (assert (vector? bindings) "expected a vector for bindings")
  (assert (even? (count bindings))) "must have an even number of forms in binding vector"
  (let [destructured (destructure bindings)
        global-defs (->> destructured
                         (partition 2)
                         (remove (comp (partial re-seq #"(vec|map)__")
                                       name
                                       first))
                         (map #(concat (list 'def) %)))]
    `(let* ~destructured
       (do 
         ~@global-defs)
       ~@body)))

#_(def-let [a 8
          {:keys [aa] :as m} {:aa 33}
          [x y] [11 12]]
    (+ x a))


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

