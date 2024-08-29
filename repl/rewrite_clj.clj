(ns rewrite-clj
  (:require [rewrite-clj.parser :as p]
            [rewrite-clj.node :as n]
            [rewrite-clj.zip :as z]
            [rewrite-clj.paredit :as rpe]
            [rewrite-clj.custom-zipper.utils :as rczu]
            [clojure.string :as str]
            [clojure.pprint :refer [pprint]]))


;; rewrite-clj ;; used in clerk, cljfmt, clojure-lsp and others
;; ;; uses zippers
;;   https://github.com/clj-commons/rewrite-clj
;; cljfmt
;; ;; used in repl-balance (deferred loading of this)
;;   https://github.com/weavejester/cljfmt
;; clj-kondo
;;   https://github.com/clj-kondo/clj-kondo
;; clojure-lsp
;; ;; uses rewrite-clj and clj-kondo
;;   https://github.com/clojure-lsp/clojure-lsp

(require '[rewrite-clj.parser :as p]
         '[rewrite-clj.node :as n]
         '[rewrite-clj.zip :as z]
         '[rewrite-clj.paredit :as rpe])

(def s "(defn f[x y]\n  (+ x 8))")
#_(def p (p/parse-string s))
#_(def z (z/of-node p {:track-position? true}))
;or
(def z (z/of-string s {:track-position? true}))

; z/next does a postwalk depth first traversal
(-> z
    (z/postwalk (fn [loc]
                  (let [n (z/node loc)]
                    (println "" (meta (z/node loc)) (str n))
                    loc)))
    z/node
    str)
;; {:row 1, :col 2, :end-row 1, :end-col 6} defn
;; {:row 1, :col 7, :end-row 1, :end-col 8} f
;; {:row 1, :col 9, :end-row 1, :end-col 10} x
;; {:row 1, :col 11, :end-row 1, :end-col 12} y
;; {:row 1, :col 8, :end-row 1, :end-col 13} [x y]
;; {:row 2, :col 4, :end-row 2, :end-col 5} +
;; {:row 2, :col 6, :end-row 2, :end-col 7} x
;; {:row 2, :col 8, :end-row 2, :end-col 9} 8
;; {:row 2, :col 3, :end-row 2, :end-col 10} (+ x 8)
;; {:row 1, :col 1, :end-row 2, :end-col 11} (defn f [x y]
;;                                             (+ x 8))

"(defn f[x y]\n  (+ x 8))"

(-> z
    (z/prewalk (fn [loc]
                 (let [n (z/node loc)]
                   (println ";; " (meta (z/node loc)) (str n))
                   loc)))
    z/node
    str)
;; {:row 1, :col 1, :end-row 2, :end-col 11} (defn f [x y]
;;                                             (+ x 8))
;; {:row 1, :col 2, :end-row 1, :end-col 6} defn
;; {:row 1, :col 7, :end-row 1, :end-col 8} f
;; {:row 1, :col 8, :end-row 1, :end-col 13} [x y]
;; {:row 1, :col 9, :end-row 1, :end-col 10} x
;; {:row 1, :col 11, :end-row 1, :end-col 12} y
;; {:row 2, :col 3, :end-row 2, :end-col 10} (+ x 8)
;; {:row 2, :col 4, :end-row 2, :end-col 5} +
;; {:row 2, :col 6, :end-row 2, :end-col 7} x
;; {:row 2, :col 8, :end-row 2, :end-col 9} 8
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

#_ (defn find-loc1 [loc cursor]
  (loop [loc loc
         position (assoc (meta (z/node loc)) :cursor 0)
         offset 0]
    (let [next-loc (z/next* loc)
          next-position (some-> (z/node next-loc)
                                (meta))]
      (do
       (println position offset (z/end? loc) (format "\"%s\"" (z/node loc)))
       (println next-position offset #_(str (z/node next-loc)))
       (println))
      (if (or (nil? next-position) #_(z/end? loc)
              (<= cursor (dec (+ offset (:col next-position)))))
        loc
        (recur next-loc
               (assoc next-position :cursor (dec (+ (:end-col position) offset)))
               (if (> (:row next-position) (:row position))
                 (+ offset (dec (:col position)))
                 offset))))))


#_(defn find-loc [loc cursor]
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
     (take-while  (complement z/end?)))


(defn row-offsets-map
  "for a string return a map of row-index->offset
  uses first-row-index=1"
  [s]
  (->> (str/split-lines s)
       (map (comp inc count))
       (reductions +)
       (map vector (iterate inc 2))
       (into {1 0})))

(defn rowoffsets
  "for a string return a vector of row offsets"
  [s]
  (->> (str/split-lines s)
       (map (comp inc count))
       (reductions +)
       (into [0])))

(defn loc->position
  "for a locator with positions show the position
  with :cursor and :end-cursor added"
  [z]
  (let [row-offsets (rowoffsets (str (z/node z)))
        add-cursor (fn [{:keys [row col end-row end-col] :as position}]
                     (assoc position
                       :cursor (+ -1 col (row-offsets row))
                       :end-cursor (+ -1 end-col (row-offsets end-row))))]
    (comp add-cursor meta z/node)))

(defn cursor->position*
  [s]
  (let [row-offsets (rowoffsets s)]
    (fn get-pos [{:keys [row col end-row end-col] :as position}]
      (assoc position
        :cursor (+ -1 col (row-offsets row))
        :end-cursor (+ -1 end-col (row-offsets end-row)))))
  )

(defn find-loc [loc target-cursor]
  (let [get-position (loc->position loc)]
    (loop [l loc]
      (let [{:keys [cursor end-cursor] :as position} (get-position l)]
        ;(println position (z/node l) (type l))
        (cond
          ;; we found it
          (= target-cursor cursor)
          l

          ;; look past this node (right not down)
          (>= target-cursor end-cursor)
          (if-let [right-sib (z/right* l)]
            (recur right-sib)
            (assoc l :inner-cursor end-cursor))

          (= target-cursor (dec end-cursor))
          (if-let [inside (z/down* l)]
            (-> (z/append-child l (n/whitespace-node " "))
                (z/down)
                (z/rightmost*))
            (assoc l :inner-cursor (- target-cursor cursor)))

          ;; descend into this node or return the fragment
          (< target-cursor end-cursor)
          (if-let [inside (z/down* l)]
            (recur inside)
            (assoc l :inner-cursor (- target-cursor cursor))))))))

(defn find-pos1*
  "given a string
  returns a function of cursor->position"
  [s]
  (fn [target]
    (loop [r 1 [row-offset & offsets] (rowoffsets s)]
      (if (>= target (first offsets))
        (recur (inc r) offsets)
        (let [col (- (inc target) row-offset)]
          {:row r :end-row r :col col :end-col (inc col)})))))

(defn find-pos*
  "given a string
  returns a function of cursor->position"
  [s]
  (let [offsets (rowoffsets s)]
    (fn [target]
      (loop [offsets (rest offsets)
             row 1
             row-offset 0]
        (let [next-offset (first offsets)]
          (if (and next-offset (>= target next-offset))
            (recur (rest offsets) (inc row) next-offset)
            ;; TODO: fix offsets running off the end
            (let [col (- (inc target) row-offset)]
              {:row row :end-row row :col col :end-col (inc col)})))))))

;; should this take a string (i.e. a buffer) rather than a loc
(defn find-pos
  "given locator and a cursor
   returns the corresponding position"
  [loc target-cursor]
  ((find-pos* (z/root-string loc)) target-cursor))

;; slower than find-loc
(defn find-loc2
  [loc target-cursor]
  (->> (find-pos loc
                 target-cursor)
       (z/find-last-by-pos loc)))

(str (z/node (find-loc z 20)))

;;; example of editing a token node
(-> "(def-let [x 3]
      (inc x))"
    (z/of-string {:track-position? true})
    (z/find-last-by-pos pos)
    ;(#'paredit/kill-at-pos pos)
    (z/edit (comp symbol #(subs % 4) str))
    z/node
    n/tag)

(defn kill
  ([] (kill j/*buffer*))
  ([buf]
   (let [s (str buf)
         cur (.cursor buf)
         cur-pos (pe/str-find-pos s cur)
         cur-beg-col (:col cur-pos)
         cur-beg-row (:row cur-pos)
         loc (-> s
                 (z/of-string {:track-position? true})
                 (z/find-last-by-pos cur-pos))
         remove? (fn [loc]
                   (-> loc z/node meta :row (= cur-beg-row)))]
     (cond
       ;; remove a newline
       (-> loc z/node n/tag #{:newline})
       (-> loc z/remove z/root-string)                      ;; remove additional whitespace or reformat?
       ;; truncate a token and remove until end of line
       (-> loc z/node n/tag #{:token})
       (let [node-beg-col (-> loc z/node meta :col)]
         (if (= node-beg-col cur-beg-col)
           (-> (z/remove* loc)
               (rczu/remove-right-while remove?)
               (z/root-string))
           (-> loc
               (z/edit (comp symbol
                             #(subs % 0 (- cur-beg-col node-beg-col))
                             str))
               (rczu/remove-right-while remove?)
               (z/root-string))))

       (-> loc z/node n/tag #{:token})
       :not-a-token))))

;; count the size of the tail that matches
(->> (map reverse ["xyzabc" "xabc"])
     (apply map vector)
     (take-while (partial apply =))
     count)


(comment

(defn f [x y] "
01234567891111
          0123"
  (+ x 8)) "
1111111222
4567890123")

(require '[repl-balance.clojure.paredit :as pe]
         '[repl-balance.clojure.paredit-test :refer [display-str+cur] :as pet]
         '[clojure.test :refer [deftest is testing] :as test]
         :reload)

(deftest barf-forward-repl-test
(let [s "[1 [:a 7 :key] :b]"]
  (doall (for  [c (range (count s))]
           (let [orig (display-str+cur s c)
                 modified (try
                            (->> (pe/barf-forward-str s c)
                                 (apply display-str+cur))
                            (catch Exception e "oops" #_(-> e Throwable->map :cause (subs 0 20)) ))]
             (testing (str "barf-forward-str with: " orig)
              (is (= "oops" modified))))))))
(test/run-test barf-forward-repl-test)
(test/run-test-var #'barf-forward-repl-test)

(require '[cljfmt.core :as fmt])
fmt/reformat-form

(-> "[x\n                                                    y]"
    (z/of-string {:track-position? true})
    ((fn [loc]
       (z/replace loc (-> loc z/node fmt/unindent (fmt/indent)))))
    z/root-string)
