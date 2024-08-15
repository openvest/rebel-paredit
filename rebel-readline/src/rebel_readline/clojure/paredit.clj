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
              ;; TODO: minor fix, on <newline> the end-row should be (inc row)
              {:row row :end-row row :col col :end-col (inc col)})))))))

(defn str-find-pos
  "given a string and an offset (i.e. cursor)
  return the position (i.e. map of [:row :col :end-row :end-col]
  Note: inverse of str-find-cursor"
  [^String s ^Integer cursor]
  ((str-find-pos* s) cursor))

(defn str-find-cursor
  "given a string and a position (i.e. map with [:row :col])
  return the cursor position as an int
  Note: inverse of str-find-pos"
  [^String s pos]
  (let [offsets (str-row-offsets s)]
    (-> (:row pos)
        dec
        (offsets)
        (+ (dec (:col pos))))))

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

(defn kill
  [s c]
  ;; like kill but works on a string & cursor rather than j/*buffer*
  ;; special case, killing after the quote we need to backspace to remove the quote
  ;; quote nodes can't NOT have 1 child
  (cond
    ; if we currently end on a closing bracket or quote, do nothing
    (or (= c (count s))
        (#{\) \} \] \"} (.charAt s (inc c))))
    [s c 0]
    ; if we currently at a line ending, remove it
    (#{\newline} (.charAt s c))
    [(str (subs s 0 c) (subs s (inc c))) c 1]
    ;; if it is just after a quote symbol process without that and add it back
    (and (> c 0) (#{\` \'} (.charAt s (dec c))))
    (let [[new-s _ cut-size] (kill (str (subs s 0 (dec c)) (subs s c)) (dec c))]
      [(str (subs s 0 c) (subs new-s (dec c))) c cut-size])
    ;; everything else is handled by rewrite-clj
    :default
    (let [cur-pos (str-find-pos s c)
          cur-beg-col (:col cur-pos)
          cur-beg-row (:row cur-pos)
          loc (-> s
                  (z/of-string {:track-position? true})
                  (z/find-last-by-pos cur-pos))
          remove? (fn [loc]
                    (let [n (-> loc z/node)]
                      (and (not= :newline (n/tag n))
                           (-> loc z/node meta :row (= cur-beg-row)))))
          new-s (cond                                     ;; should this be a multifun
                  ;; truncate a token and remove until end of line
                  (-> loc z/node n/tag #{:token :list :vector})
                  (let [node-beg-col (-> loc z/node meta :col)]
                    (if (= node-beg-col cur-beg-col)
                      (-> loc
                          (rczu/remove-right-while remove?)
                          (z/remove*)
                          (z/root-string))
                      (-> loc
                          (rczu/remove-right-while remove?)
                          (z/edit (comp symbol
                                        #(subs % 0 (- cur-beg-col node-beg-col))
                                        str))
                          (z/root-string))))
                  ;; truncate a whitespace node and remove until end of line
                  (-> loc z/node n/tag #{:whitespace})
                  (let [node-beg-col (-> loc z/node meta :col)]
                    (if (= node-beg-col cur-beg-col)
                      (-> loc
                          (rczu/remove-right-while remove?)
                          (z/remove)
                          (z/root-string))
                      (-> loc
                          (rczu/remove-right-while remove?)
                          (z/replace (n/spaces (- cur-beg-col node-beg-col)))
                          (z/root-string))))

                  :default #_(-> loc z/node n/tag #{:token})
                  (z/root-string loc))]
      [new-s c (- (count s) (count new-s))])))

(defn kill-in-buff
  []
  (let [s (str j/*buffer*)
        c (.cursor j/*buffer*)
        [new-s new-c cut-len] (kill s c)
        kill-str (subs s c (+ c cut-len))]
    (when (not-empty kill-str)  #_ (re-find #"[^\s]" kill-str) ;;  exclude all whitespace from killRing?
      (j/add-to-killRing kill-str))
    (.delete j/*buffer* cut-len)))

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

;; slurp and barf

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

;; splice and split

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

;; movement functions

(defn forward
  ([] (forward j/*buffer*))
  ([buf] (doto buf
           (.cursor (forward (str buf) (.cursor buf)))))
  ([s cur]
   (if (= cur (count s))
     ;; if we are at the end, do nothing
     cur
     (let [cursor-pos (str-find-pos s cur)
           loc (-> s
                   (z/of-string {:track-position? true})
                   (z/find-last-by-pos cursor-pos)
                   (z/skip-whitespace))
           node-pos (-> loc
                        z/node
                        meta)]
       (str-find-cursor s
                        {:row (:end-row node-pos)
                         :col (:end-col node-pos)})))))

(defn backward
  ([] (backward j/*buffer*))
  ([buf] (doto buf
           (.cursor (backward (str buf) (.cursor buf)))))
  ([s cur]
   (if (or (= cur 0)                                        ;beg of str
           (= cur (count s)))                               ;end of str TODO: fails on multiple forms e.g. "(foo)(bar)|"
     ;; if we are at the beginning, do nothing
     0
     (let [cursor-pos (str-find-pos s cur)
           orig-loc (-> s
                        (z/of-string {:track-position? true})
                        (z/find-last-by-pos cursor-pos))
           orig-pos (-> orig-loc z/node meta)
           left-loc (cond
                      (= (select-keys cursor-pos [:row :col])
                         (-> orig-loc z/node meta
                             (select-keys [:row :col])))
                      (if (z/leftmost? orig-loc)
                        (z/up orig-loc)
                        (z/left orig-loc))

                      (and (= (:row cursor-pos) (:end-row orig-pos))
                           (= (:end-col cursor-pos) (:end-col orig-pos))
                           (-> orig-loc z/node :children seq))
                      (-> orig-loc z/down z/rightmost)

                      :default
                      (z/skip-whitespace-left orig-loc))
           node-pos (-> left-loc
                        z/node
                        meta)]
       (str-find-cursor s
                        {:row (:row node-pos)
                         :col (:col node-pos)})))))
