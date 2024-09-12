(ns repl-balance.zip-utils
  (:require [cljfmt.core :as fmt]
            [rewrite-clj.node :as n]
            [rewrite-clj.zip :as z]))


;; TODO:  try to find out if this is needed.  is there an optional
;;  argument to fmt/indent that will do this
;;  this very much feels like reinventing the wheel.
;;  and as long as this is it may be buggy yet
(defn re-indent-form
  "adds space to the indentation of a locator
  will use the (-> form meta :col) to add indentation to
  the result of cljfmt/indent"
  [form]
  (let [indent (or (some-> form meta :col dec) 0)
        spaces (apply str (repeat indent \space))
        loc (-> form
                fmt/unindent
                fmt/indent
                z/of-node)]
    (if-not (pos? indent)
      (z/root loc)
      (loop [loc loc
             should-add false]
        (if (z/end? loc)
          (z/root loc)
          (let [tag (z/tag loc)]
            (cond
              ;; newline
              (= tag :newline)
              (recur (z/next* loc) true)
              ;; add to whitespace
              (and should-add
                   (= tag :whitespace))
              (-> loc
                  (z/replace (-> (z/string loc)
                                 (str spaces)
                                 (n/whitespace-node)))
                  (z/next*)
                  (recur false))
              ;; add space in front of this node
              (and should-add)
              (-> (z/insert-left loc spaces)
                  (z/next*)
                  (recur false))
              :default
              (recur (z/next* loc) false))))))))
