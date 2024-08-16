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
                   ;closer without an opener
                   (empty? opens) (reduced [paren])
                   ;;oops the closer->opener match is not good
                   :else (reduced opens)))
               nil)
       first))

;; note that rebel-readline.clojure.sexp/find-open-sexp-start
;; gets this wrong
(-> (tokenize/tag-sexp-traversal "(foo [bar [x])")
    (SUT/find-open-sexp-start 100))

(->> (tokenize/tag-sexp-traversal "(foo [bar [x])")
     (reduce (fn [stack tag]
               (cond
                 ; new opener
                 (open->close (peek tag))
                 (conj stack tag)
                 ; successful closer
                 (-> tag peek close->open (= (last (first stack))))
                 (rest stack)
                 ;; closer without an opener
                 (-> tag peek close->open)
                 (stack conj ["x" ])
                 :default stack)) nil))
(def open->close
 (->> delims
      (map
        (fn [d]
          (let [d-name (name d)]
            (mapv keyword [(str "open-" d-name)
                           (str "close-" d-name)]))))
      (into {})))

(def close->open
  (->> open->close
       (map (comp vec reverse))
       (into {})))

(->> (tokenize/tag-sexp-traversal "(let[foo (bar foo))")
     (reduce (fn [stack tag]
               (cond
                 ; new opener
                 (open->close (peek tag))
                 (do (println "open" tag) (conj stack tag))
                 ; successful closer
                 (some-> tag peek close->open (= (peek (first (drop-while (comp :missing-opener peek) stack)))))
                 (do (println "close" tag stack) (rest stack))
                 ;; closer without an opener
                 (some-> tag peek close->open)
                 (do (println "missing open" tag) (concat stack [["x" (nth (first stack) 2) (second tag) :missing-opener]]))
                 :default (do (println "default") stack))) nil))
