(ns repl.sexp
  (:require [rebel-readline.clojure.sexp :as SUT]
            [rebel-readline.clojure.tokenizer :as tokenize]))

;; ref: rebel-readline.clojure.sexp
(def delims #{:bracket :brace :paren :quote})
#_(def openers (->> delims
                  (map #(->> % name (str "open-") keyword))
                  (into #{})))
(def closers (->> delims
                  (map (fn [d] [(->> d name (str "close-") keyword)
                                (->> d name (str "open-") keyword)]))
                  (into {})))
(def openers (->> closers (map (comp vec reverse))
                  (into {})))

(def s "(defn f[x y]\n  (+ x 8))")

(defn open-paren
  [s]
  (->> (tokenize/tag-sexp-traversal s)
       (reduce (fn [opens [_p _beg _end k :as paren]]
                 (cond
                   ;if this is an opener
                   (openers k) (conj opens paren)
                   ;if this is a successful closer
                   (some-> opens first (nth 3)
                           (= (closers k)))
                   (rest opens)
                   ;;
                   (empty? opens) (reduced [paren])
                   ;;oops the closer->opener match is not good
                   :else (reduced opens)))
               nil)
       first))

