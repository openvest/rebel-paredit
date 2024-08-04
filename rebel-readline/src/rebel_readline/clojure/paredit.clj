(ns rebel-readline.clojure.paredit
  (:require [rebel-readline.jline-api :as j]
            [rewrite-clj.paredit :as pe]
            [rewrite-clj.node :as n]
            [rewrite-clj.zip :as z]
            [clojure.string :as str]))

;; string based functions

(defn str-row-offsets
  "for a string return a vector of row offsets"
  [^String s]
  (->> (str/split-lines s)
       (map (comp inc count))
       (reductions +)
       (into [0])))

(defn str-find-pos*
  "given a string
  returns a function of cursor->position"
  [^String s]
  (let [offsets (str-row-offsets s)]
    (fn s-find-pos* [target]
      (loop [offsets (rest offsets)
             row 1
             row-offset 0]
        (let [next-offset (first offsets)]
          (if (and next-offset (>= target next-offset))
            (recur (rest offsets) (inc row) next-offset)
            ;; TODO: fix offsets running off the end
            (let [col (- (inc target) row-offset)]
              {:row row :end-row row :col col :end-col (inc col)})))))))


(defn str-find-pos
  "give a string and an offset (i.e. cursor)
  return the position (i.e. map of [:row :col :end-row :end-col]"
  [s cursor]
  ((str-find-pos* s) cursor))

;; zipper/locator based functions
(defn loc->position*
  "for a locator with positions
  return a function that takes a locator from within the root
         and returns the position with :cursor and :end-cursor added"
  [z]
  (let [row-offsets (str-row-offsets (str (z/node z)))
        add-cursor (fn [{:keys [row col end-row end-col] :as position}]
                     (assoc position
                       :cursor (+ -1 col (get row-offsets (dec row)))
                       :end-cursor (+ -1 end-col (row-offsets (dec end-row)))))]
    (comp add-cursor meta z/node)))`

(defn loc->position
  "for a locator with positions
  return the position with :cursor and :end-cursor added"
  [z]
  ((loc->position* z) z))

(defn find-loc [loc target-cursor]
  "given a zipper/loc with {:track-position true}
  and a cursor (offset into the root-string)
  return the locator of the node at the cursor"
  (let [get-position (loc->position* loc)]
    (loop [l loc]
      (let [{:keys [cursor end-cursor] :as position} (get-position l)]
        (println ">>>" position (z/node l) " type: " (type l))
        (cond
          ;; we found it
          (= target-cursor cursor)
          l
          ;; look past this node (right not down)
          (>= target-cursor end-cursor)
          (if-let [right-sib (z/right* l)]
            (recur right-sib)
            (assoc l :inner-cursor end-cursor))
          ;; we are sitting on the end
          (= target-cursor (dec end-cursor))
          (if (z/down* l)
            ;; do we really want to add a node here?
            (-> (z/append-child l (n/whitespace-node " "))
                (z/down)
                (z/rightmost*))
            (assoc l :inner-cursor (- target-cursor cursor)))
          ;; descend into this node or return the fragment
          (< target-cursor end-cursor)
          (do
            (println "---> inside down movement" target-cursor cursor)
            (if-let [inside (z/down* l)]
              (recur inside)
              (assoc l :inner-cursor (- target-cursor cursor)))))))))

;; buffer based functions
;; killing

;; note that this kills more than one line
;; which is different from other systems
(defn kill
  "For a Buffer, kill at a cursor position."
  ([] (kill j/*buffer*))
  ([buf]
   (if (#{\) \} \] \"} (char (.nextChar buf)))
     ; if we currently end on a closing bracket or quote, do nothing
     buf
     (let [s (str buf)
           cur (.cursor buf)
           pos (str-find-pos s cur)
           tail (-> s
                    (z/of-string {:track-position? true})
                    (pe/kill-at-pos pos)                    ;kill-at-pos or kill-one-at-pos
                    (z/root-string)
                    (subs cur))]
       (doto buf
         (.cursor cur)
         (.write tail)
         (.delete (- (.length buf)
                     (.cursor buf)))
         (.cursor cur))))))

(defn slurp-forward
  "For a Buffer, slurp forward"
  [buf]
  (let [cur (.cursor buf)
        s (str buf)
        pos (str-find-pos s cur)
        tail (-> s
                 (z/of-string {:track-position? true})
                 (z/find-last-by-pos pos)
                 (pe/slurp-forward)
                 (z/root-string)
                 (subs cur))]
    (doto buf
      (.write tail)
      (.delete (- (.length buf)
                  (.cursor buf)))
      (.cursor cur))))

(defn barf-forward
  "For a Buffer, barf forward"
  [buf]
  (let [cur (.cursor buf)
        s (str buf)
        pos (str-find-pos s cur)
        tail (-> s
                 (z/of-string {:track-position? true})
                 (z/find-last-by-pos pos)
                 (pe/barf-forward)
                 (z/root-string)
                 (subs cur))]
    (doto buf
      (.write tail)
      (.delete (- (.length buf)
                  (.cursor buf)))
      (.cursor cur))))

(defn slurp-backward
  "For a Buffer, slurp backward"
  [buf]
  (let [cur (.cursor buf)
        s (str buf)
        old-tail-len (- (count s) cur)
        pos (str-find-pos s cur)
        new-s (-> s
                  (z/of-string {:track-position? true})
                  (z/find-last-by-pos pos)
                  (pe/slurp-backward)
                  (z/root-string))]
    (doto buf
      (.clear)
      (.write new-s)
      (.cursor (- (count new-s) old-tail-len)))))

(defn barf-backward
  "For a Buffer, barf backward"
  [buf]
  (let [cur (.cursor buf)
        s (str buf)
        old-tail-len (- (count s) cur)
        pos (str-find-pos s cur)
        new-s (-> s
                  (z/of-string {:track-position? true})
                  (z/find-last-by-pos pos)
                  (pe/barf-backward)
                  (z/root-string))]
    (doto buf
      (.clear)
      (.write new-s)
      (.cursor (- (count new-s) old-tail-len)))))


