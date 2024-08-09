(ns rebel-readline.clojure.paredit
  (:require [rebel-readline.jline-api :as j]
            [rewrite-clj.paredit :as pe]
            [rewrite-clj.node :as n]
            [rewrite-clj.zip :as z]
            [rewrite-clj.custom-zipper.utils :as rczu]
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
;; TODO: kill should operate on strings and not buffers so we can use the line-reader kill ring
(defn kill
  []
  ;; special case, killing after the quote we need to backspace to remove the quote
  ;; quote nodes can't not have 1 child
  ;; should we add it back in at the end?
  (when (-> j/*buffer* (.prevChar) char #{\`\'})
    (.backspace j/*buffer*))
  (if (#{\) \} \] \"} (char (.nextChar j/*buffer*)))              ;; add (char 0) ??
    ; if we currently end on a closing bracket or quote, do nothing
    j/*buffer*
   (let [buf j/*buffer*
         s (str buf)
         cur (.cursor buf)
         cur-pos (str-find-pos s cur)
         cur-beg-col (:col cur-pos)
         cur-beg-row (:row cur-pos)
         loc (-> s
                 (z/of-string {:track-position? true})
                 (z/find-last-by-pos cur-pos))
         remove? (fn [loc]
                   (let [n (-> loc z/node)]
                     (and (not= :newline (n/tag n))
                          (-> loc z/node meta :row (= cur-beg-row)))))
         new-s (cond                                        ;; should this be a multifun
                 ;; remove a newline
                 (-> loc z/node n/tag #{:newline})
                 (-> loc z/remove z/root-string)            ;; remove additional whitespace or reformat?
                 ;; truncate a token and remove until end of line
                 (-> loc z/node n/tag #{:token :list :vector})
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
                 ;; truncate a whitespace node and remove until end of line
                 (-> loc z/node n/tag #{:whitespace})
                 (let [node-beg-col (-> loc z/node meta :col)]
                   (if (= node-beg-col cur-beg-col)
                     (-> (z/remove* loc)
                         (rczu/remove-right-while remove?)
                         (z/root-string))
                     (-> loc
                         (z/replace (n/spaces (- cur-beg-col node-beg-col)))
                         (rczu/remove-right-while remove?)
                         (z/root-string))))

                 :default #_(-> loc z/node n/tag #{:token})
                 (z/root-string loc))]
     (doto buf
       (.cursor cur)
       (.write (subs new-s cur))
       (.delete (- (.length buf)
                   (.cursor buf)))
       (.cursor cur)))))


(defn kill-all
  "For a Buffer, kill at a cursor position."
  ([] (kill-all j/*buffer*))
  ([buf]
   (if (#{\) \} \] \"} (char (.nextChar buf)))              ;; add (char 0) ??
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
  ([] (slurp-forward j/*buffer*))
  ([buf]
   (when (#{\]\)\}} (char(.currChar buf)))
     (doto buf
       (.write " ")
       (.move -1)))
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
       (.cursor cur)))))

(defn barf-forward
  "For a Buffer, barf forward"
  ([] (barf-forward j/*buffer*))
  ([buf]
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
       (.cursor cur)))))

(defn slurp-backward
  "For a Buffer, slurp backward"
  ([] (slurp-backward j/*buffer*))
  ([buf]
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
       (.cursor (- (count new-s) old-tail-len))))))

(defn barf-backward
  "For a Buffer, barf backward"
  ([] (barf-backward j/*buffer*))
  ([buf]
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
       (.cursor (- (count new-s) old-tail-len))))))

(defn open-and-slurp
  "insert open paren and slurp forward"
  ([] (open-and-slurp j/*buffer*))
  ([buf]
   (doto buf
     (.write "()")
     (.move -1))
   (slurp-forward buf)
   (.delete buf 1)
   ;; should this be part of slurp
   #_(when (#{\(\[\{} (.currChar buf))
     (doto buf
       (.write " ")
       (.move -1)))))

(defn splice
  "splice the list/vect"
  ([] (splice j/*buffer*))
  ([buf]
   (let [cur (.cursor buf)
         s   (str buf)
         pos (str-find-pos s cur)
         new-s (-> s
                   (z/of-string {:track-position? true})
                   (z/find-last-by-pos pos)
                   ((fn [loc]
                      (if (#{\}\)\]} (char (.currChar buf)))
                        loc
                        (z/up loc))))
                   (pe/splice)
                   (z/root-string))]
     (doto buf
       (.clear)
       (.write new-s)
       (.cursor cur)))))

(defn split
  "split the list/vect"
  ([] (split j/*buffer*))
  ([buf]
   (let [cur (.cursor buf)
         s   (str buf)
         pos (str-find-pos s cur)
         new-s (-> s
                   (z/of-string {:track-position? true})
                   (z/find-last-by-pos pos)
                   (pe/split-at-pos pos)
                   (z/root-string))]
     (doto buf
       (.clear)
       (.write new-s)
       (.cursor cur)))))
