(ns lsp
  (:require [clojure.string :as str]))


;; rewrite-clj ;; used in clerk, cljfmt, clojure-lsp and others
;; ;; uses zippers
;;   https://github.com/clj-commons/rewrite-clj
;; cljfmt
;; ;; used in rebel-readline (deferred loading of this)
;;   https://github.com/weavejester/cljfmt
;; clj-kondo
;;   https://github.com/clj-kondo/clj-kondo
;; clojure-lsp
;; ;; uses rewrite-clj and clj-kondo
;;   https://github.com/clojure-lsp/clojure-lsp

;; colorizing specific
;; puget
;;  https://github.com/greglook/puget
;; note that rebel-readline has its own tokenizer and colorizer
(require '[rewrite-clj.parser :as p]
         '[rewrite-clj.node :as n]
         '[rewrite-clj.zip :as z]
         '[rewrite-clj.paredit :as paredit])

(def s "(defn f[x y]\n  (+ x 8))")
(def p (p/parse-string s))
(def z (z/of-node p {:track-position? true}))

; z/next does a postwalk depth first traversal
(-> z
    (z/postwalk (fn [loc]
                  (let [n (z/node loc)]
                    (println "#_=> " (meta (z/node loc)) (str n))
                    loc)))
    z/node
    str)
#_=> {:row 1, :col 2, :end-row 1, :end-col 6} defn
#_=> {:row 1, :col 7, :end-row 1, :end-col 8} f
#_=> {:row 1, :col 9, :end-row 1, :end-col 10} x
#_=> {:row 1, :col 11, :end-row 1, :end-col 12} y
#_=> {:row 1, :col 8, :end-row 1, :end-col 13} [x y]
#_=> {:row 2, :col 4, :end-row 2, :end-col 5} +
#_=> {:row 2, :col 6, :end-row 2, :end-col 7} x
#_=> {:row 2, :col 8, :end-row 2, :end-col 9} 8
#_=> {:row 2, :col 3, :end-row 2, :end-col 10} (+ x 8)
#_=> {:row 1, :col 1, :end-row 2, :end-col 11} (defn f [x y]
                                                 (+ x 8))
"(defn f[x y]\n  (+ x 8))"

(-> z
    (z/prewalk (fn [loc]
                 (let [n (z/node loc)]
                   (println "#_=> " (meta (z/node loc)) (str n))
                   loc)))
    z/node
    str)
#_=> {:row 1, :col 1, :end-row 2, :end-col 11} (defn f [x y]
                                                 (+ x 8))
#_=> {:row 1, :col 2, :end-row 1, :end-col 6} defn
#_=> {:row 1, :col 7, :end-row 1, :end-col 8} f
#_=> {:row 1, :col 8, :end-row 1, :end-col 13} [x y]
#_=> {:row 1, :col 9, :end-row 1, :end-col 10} x
#_=> {:row 1, :col 11, :end-row 1, :end-col 12} y
#_=> {:row 2, :col 3, :end-row 2, :end-col 10} (+ x 8)
#_=> {:row 2, :col 4, :end-row 2, :end-col 5} +
#_=> {:row 2, :col 6, :end-row 2, :end-col 7} x
#_=> {:row 2, :col 8, :end-row 2, :end-col 9} 8
"(defn f[x y]\n  (+ x 8))"

(defn walk [loc]
  (if-not (z/end? loc)
    (cons loc (walk (z/next loc)))))

(map (comp meta z/node) (walk z))

(defn walk-all [loc]
  (if-not (z/end? loc)
    ;; z/next* includes whitespace nodes
    (cons loc (walk-all (z/next* loc)))))

(->> (walk-all z)
     (map (fn [loc]
            (let [node (z/node loc)]
              (assoc (meta node) :str (str node)))))
     pprint)

(defn find-loc [loc cursor]
  (loop [loc loc
         position (assoc (meta (z/node loc)) :cursor 0)
         offset 0]
    (let [next-loc (z/next* loc)
          next-position (some-> (z/node next-loc)
                                (meta))]
      (println position offset (z/end? loc) (format "\"%s\"" (z/node loc)))
      (println next-position offset #_(str (z/node next-loc)))
      (println)
      (if (or (nil? next-position) #_(z/end? loc)
              (<= cursor (dec (+ offset (:col next-position)))))
        loc
        (recur next-loc
               (assoc next-position :cursor (dec (+ (:end-col position) offset)))
               (if (> (:row next-position) (:row position))
                 (+ offset (dec (:col position)))
                 offset))))))

(str (z/node (find-loc z 20)))                              ;; correct but higher cursors all return the same loc

(defn row-offsets [s]
  (->> (str/split-lines s)
       (map (comp inc count))
       (reductions +)
       (map vector (range 2 100000))
       (into {1 0})))


(defn find-loc2 [loc cursor]
  (loop [loc loc
         position (assoc (meta (z/node loc)) :cursor 0)
         offset 0]
    (let [next-loc (z/next* loc)
          next-position (some-> (z/node next-loc)
                                (meta))]
      ;(println position offset (z/end? loc) (format "\"%s\"" (z/node loc)))
      ;(println next-position offset #_(str (z/node next-loc)))
      ;(println)
      (if (or (nil? next-position) #_(z/end? loc)
              (<= cursor (dec (+ offset (:col next-position)))))
        loc
        (recur next-loc
               (assoc next-position :cursor (dec (+ (:end-col position) offset)))
               (if (> (:row next-position) (:row position))
                 (+ offset (dec (:col position)))
                 offset))))))

(->> (iterate z/next z)
     (map #(not (z/end? %)))
     (take 10))


(defn row-offsets [s]
  (->> (str/split-lines s)
       (map (comp inc count))
       (reductions +)
       (map vector (range 2 100000))
       (into {1 0})))

(defn loc->position
  "for a locator with positions show the position with :cursor and :end-cursor added"
  [z]
  (let [row-offsets (row-offsets (str (z/node z)))
        add-cursor (fn [{:keys [row col end-row end-col] :as position}]
                     (assoc position
                       :cursor (+ -1 col (row-offsets row))
                       :end-cursor (+ -1 end-col (row-offsets end-row))))]
    (comp add-cursor meta z/node)))

(defn find-loc3 [loc target-cursor]
  (let [get-position (loc->position loc)]
    (loop [l loc]
      (let [{:keys [cursor end-cursor] :as position}  (get-position l)]
        (println position (z/node l))
        (cond
          ;; we found it
          (= target-cursor cursor) l
          ;; look past this node (right not down)
          (> target-cursor end-cursor)
          (recur (z/right* l))
          ;; TODO what if this is inside a string or comment?
          (< target-cursor end-cursor)
          (recur (z/next* l))
          ;; it's at the end of this node e.g. a vector or map
          ;; append a blank whitespace and navigate to that
          (and (= target-cursor end-cursor) (n/inner? (z/node l)))
          (-> (z/append-child l (n/whitespace-node " "))
              (z/down)
              (z/rightmost*))
          ;; inside a fragment of this node
          (= target-cursor end-cursor)
          (assoc l :inner-cursor (- target-cursor cursor)))))))
