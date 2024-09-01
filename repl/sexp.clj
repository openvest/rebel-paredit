(ns repl.sexp
  (:require [repl-balance.clojure.sexp :as SUT]
            [repl-balance.clojure.tokenizer :as tokenize]
            [repl-balance.test-helpers :refer [split-s-cur]]
            [clojure.string :as str]))

;; ref: repl-balance.clojure.sexp
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

;; note that repl-balance.clojure.sexp/find-open-sexp-start
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

;; counting the spaces before and after a new-line
;; look for a better way i think
(let [s "hi   \n  there"
      cur (str/index-of s \newline)]
  [(->> (for [i (range (dec cur) 0 -1)]
          (.charAt s i))
        (take-while #(= \space %))
        count) 
   (->> (for [i (range (inc cur) (count s))]
          (.charAt s i))
        (take-while #(= \space %))
        count)])
;; counting spaced before and after 
(let [s "hi   \n  there"
      cur (str/index-of s \newline)]
  [(count (take-while #{\space} (reverse (subs s 0 cur))))
   (count (take-while #{\space} (subs s (inc cur))))])
;; try a regex version?
(let [s "hi   \n  there"
      cur (str/index-of s \newline)]
  [(count (re-find #" *$" (subs s 0 cur)))
   (count (re-find #"^ *" (subs s (inc cur) )))])

;; multiline
(re-seq #"(?s)\s*\n\s*" "hi \n  there")
(re-matches #"(?s)[^ ]*(\s*\n\s*).*" "hi\n \n there")



(let [[s cur] (split-s-cur "hi|\nthere")
      next-newline (or (str/index-of s \newline cur) (count s))
      tokens (tokenize/tag-sexp-traversal s)]
  (if-let [end-paren (some-> tokens
                             (SUT/find-open-sexp-start next-newline)
                             ((fn [[_ beg end _]]
                                (when (>= beg cur)
                                  (-> (SUT/find-open-sexp-end tokens next-newline)
                                      (nth 2))))))]
    [(str (subs s 0 cur) (subs s end-paren))]
    [(str (subs s 0 cur) (subs s next-newline))]))

(defn k1
  [s cur]
  (let [kill-end (or (str/index-of s \newline cur) (count s))
        tokens (tokenize/tag-sexp-traversal s)
        trim-end (fn [end]
                   (->> (count (re-find #"^ *" (subs s (inc end) )))
                        (+ end)
                        (subs s )))]
    (->> (or (some-> tokens
                    (SUT/find-open-sexp-start kill-end)
                    ((fn [[_ beg end _]]
                       (when (>= beg cur)
                         (-> (SUT/find-open-sexp-end tokens kill-end)
                             (nth 2))))))
             kill-end)
         trim-end
         (str (subs s 0 cur)))))
