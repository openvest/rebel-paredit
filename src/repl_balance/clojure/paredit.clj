(ns repl-balance.clojure.paredit
  (:require [repl-balance.clojure.sexp :as sexp]
            [repl-balance.jline-api :as j]
            [cljfmt.core :as fmt]
            [rewrite-clj.paredit :as pe]
            [rewrite-clj.node :as n]
            [rewrite-clj.zip :as z]
            [rewrite-clj.custom-zipper.utils :as rczu]
            [repl-balance.clojure.tokenizer :as tokenize]
            [clojure.string :as str]
            [clojure.edn :as edn])
  (:import [org.jline.reader.impl LineReaderImpl BufferImpl]))

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
  [^String s pos & at-end?]
  (let [offsets (str-row-offsets s)
        row-offset (-> (:row pos)
                       dec
                       (offsets))]
    (if (first at-end?)
      (+ row-offset (dec (:end-col pos)))
      (+ row-offset (dec (:col pos))))))

(defn str-find-cursor-range
  "given a string and a position (i.e. map with [:row :col])
  return the cursor position as an int
  Note: inverse of str-find-pos"
  [^String s pos]
  (let [offsets (str-row-offsets s)
        row-offset (-> (:row pos)
                       dec
                       (offsets))]
    [(+ row-offset (dec (:col pos)))
     (+ row-offset (dec (:end-col pos)))]))

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

(defn coll-end? [loc pos]
  "for a locator and a position (e.g. position of a cursor)
  check whither position is at the `tail`.
  `tail` here means part of the collection but after the last child"
  (let [node-pos (-> loc z/node meta)]
    (and (#{:vector :set :list :map :forms} (z/tag loc))
         (= (:end-row pos) (:end-row node-pos))
         (= (:end-col pos) (:end-col node-pos))
         loc)))

(defn movement
  "helper function that applies movements to a locator
  like using -> but is usable like
  `(condp movement loc
    [z/down z/right] [:some return]
    [z/down z/down] :>> #(do-something-with-result %))`
  short-circuits on first `nil`"
  [movements loc]
  (reduce (fn [loc move] (or (move loc) (reduced nil)))
          loc
          movements))

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

(defmulti truncate-node
          "if we are inside a string, symbol or whitespace,
          truncate the node to the specified length"
          (fn [v _length]
            (cond
              (string? v) :string
              (symbol? v) :symbol
              (re-matches #"\s*" v) :whitespace)))

(defmethod truncate-node :symbol
  [node length]
  (-> node str (subs 0 length) symbol n/coerce))

(defmethod truncate-node :string
  [node length]
  (-> node (subs 0 length) n/coerce))

(defmethod truncate-node :whitespace
  [_node length]
  (n/spaces length))

(defn truncate [loc len]
  (cond
    (z/sexpr-able? loc) (z/edit loc #(truncate-node % len))
    (= :whitespace (z/tag loc)) (z/replace loc (n/spaces len))
    :default (do (println "can't truncate " (z/node loc))
                 loc)))

;; buffer based functions
;; killing

(defn kill-orig
  "kill string up to the next closing delimiter or up to newline"
  [^String s ^Integer c]
  (cond
    ; if we currently end on a closing bracket or quote, do nothing
    (or (= c (count s))
        (#{\) \} \]} (.charAt s c))
        ;; all of this to handle (foo |"bar")  :-(
        ;; Using tokenize-sexp-traversal here and zippers further down??
        ;; Keeping both while considering  moving to tag-sexp-traversal.
        ;; tokenize-sexp-traversal may be simpler, faster and more fault-tolerant.
        (and (= (.charAt s c) \")
             (->> (tokenize/tag-sexp-traversal s)
                  (drop-while (fn [[_ beg _ _]] (< beg c)))
                  (first)
                  ((fn [[_ beg _ tag]]
                     (and (= beg c)
                          (str/starts-with? (str tag) ":close-")))))))
    [s c 0]
    ; if we currently at a line ending, remove it
    (#{\newline} (.charAt s c))
    [(str (subs s 0 c) (subs s (inc c))) c 1]
    ;; if it is just after a quote symbol process without that and add it back
    ;; quote nodes MUST have 1 child
    (and (> c 0) (#{\` \' \@} (.charAt s (dec c))))
    (let [[new-s _ cut-size] (kill-orig (str (subs s 0 (dec c)) (subs s c)) (dec c))]
      [(str (subs s 0 c) (subs new-s (dec c))) c cut-size])
    ;; everything else is handled by rewrite-clj
    :default
    (let [cur-pos (str-find-pos s c)
          loc (-> s
                  (z/of-string {:track-position? true})
                  (z/find-last-by-pos cur-pos))
          node (-> loc z/node)
          node-pos (meta node)
          remove? (fn [loc]
                    (let [n (-> loc z/node)]
                      (and (not= :newline (n/tag n))
                           (-> loc z/node meta :row (= (:row cur-pos))))))
          new-s (cond
                  ;; st the beginning of something, remove right siblings then remove self
                  (and (= (:row node-pos) (:row cur-pos))
                       (= (:col node-pos) (:col cur-pos)))
                  (-> loc
                      (rczu/remove-right-while remove?)
                      (z/remove*)
                      (z/root-string))
                  ;; if we are inside a string
                  (and (= (z/tag loc) :token)
                       (string? (z/sexpr loc)))
                  (str (subs s 0 c) (subs s (- (str-find-cursor s node-pos :at-end) 1)))
                  :default
                  (-> loc
                      (rczu/remove-right-while remove?)
                      (truncate (- (:col cur-pos) (:col node-pos)))
                      (z/root-string)))]
      [new-s c (- (count s) (count new-s))])))

(defn kill
  "kill string up to the next closing delimiter or up to newline"
  [^String s ^Integer cur]
  (let [buff-len (count s)]
   (cond
     ;; kill everything
     ;; TODO: if balanced, remove one form
     (= cur 0)
     ["" cur buff-len]
     ;; at end of buffer so do nothing
     (= cur buff-len)
     [s cur 0]
     ; if we currently at a line ending, remove it (and next indent)
     ;; should we add a space if there is not one preceding
     (= \newline (.charAt s cur))
     (let [cut-size (->> (subs s (inc cur)) (re-find #"^\s*") count inc)]
       [(str (subs s 0 cur) (subs s (+ cur cut-size))) cur cut-size])
     :default
     (let [next-newline (or (str/index-of s \newline cur) buff-len)
           tokens (tokenize/tag-sexp-traversal s)
           [_ beg-beg end-beg _] (sexp/find-open-sexp-start tokens next-newline)
           kill-end (if (some-> beg-beg (>= cur))
                      (-> (sexp/find-open-sexp-end tokens end-beg)
                          (nth 2))
                      (min next-newline (or (second (sexp/find-open-sexp-end tokens cur))
                                            buff-len)))
           extra-kill (->> (subs s kill-end)
                           (re-find #"^ *")
                           count
                           (+ kill-end))]
       [(str (subs s 0 cur)
             (subs s extra-kill))
        cur  (- extra-kill cur)]))))

(defn kill-in-buff
  []
  (let [s (str j/*buffer*)
        c (.cursor j/*buffer*)
        [_new-s _new-c cut-len] (kill s c)
        kill-str (subs s c (+ c cut-len))]
    (when (not-empty kill-str)  #_(re-find #"[^\s]" kill-str) ;;  exclude all whitespace from killRing?
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
                  z/up
                  ((fn [loc]
                     (z/replace loc (-> loc z/node fmt/unindent (fmt/indent)))))
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
         loc (-> s
                 (z/of-string {:track-position? true})
                 (z/find-last-by-pos pos)
                 (z/skip-whitespace))
         new-s (if (coll-end? loc pos)
                 (if-let [barfee (-> loc z/down z/rightmost)]
                   (->> barfee pe/barf-forward z/root-string)
                   s)
                 (->> loc pe/barf-forward z/root-string))
         new-cur (condp movement loc
                   ;; at tail with multiple children
                   [#(coll-end? % pos) z/down z/rightmost z/left]
                   :>> #(str-find-cursor s (-> % z/node meta) :at-end)
                   ;; at tail with one child or no children
                   [#(coll-end? % pos)]
                   cur
                   ;; inside at the last child
                   [z/rightmost?]
                   (if-let [left-sib (z/left loc)]
                     (str-find-cursor s (-> left-sib z/node meta) :at-end)
                     (inc (str-find-cursor s pos)))
                   ;; inside with a right sibling, no cursor change
                   []
                   cur)]
     (doto buf
       (.clear)
       (.write new-s)
       (.cursor new-cur)))))



(defn wrap-loc
  "helper to allow calling a method that takes [str cur]
  to be callable with a locator"
  [loc s-fn]
  (let [root-s (z/root-string loc)]
    (->> loc
         z/node
         meta
         (str-find-cursor root-s)
         (s-fn root-s))))


(defn barf-forward-str
  "For a Buffer, barf forward"
  ([] (barf-forward j/*buffer*))
  ([buf] (barf-forward-str (str buf) (.cursor buf)))
  ([s cur]
   (let [cur-pos (str-find-pos s cur)
         z (z/of-string s {:track-position? true})
         loc (z/find-last-by-pos z cur-pos)
         node (z/node loc)
         node-pos (meta node)]
     (cond
       (coll-end? loc cur-pos)
       ;; the cursor is on the end of a collection (i.e. at the end delimiter)
       ;; zipper edge case so lots of logic here :-(
       (condp movement loc
         ; nothing in it so do nothing
         [z/down nil?] [s cur #_nil]
         ; use rewrite-clj.paredit and set the cursor to the end of the second to last child
         [z/down z/rightmost] :>> (fn [rightmost]
                                    (let  [new-cur
                                           (if (z/leftmost? rightmost)
                                             (inc (str-find-cursor s node-pos))
                                             (-> rightmost z/left z/node meta
                                                 (#(str-find-cursor s % :at-end)))) ]
                                      [(-> rightmost pe/barf-forward z/root-string)
                                       new-cur])))
       ;; special case we are at the top of forms so do nothing
       (-> loc z/up z/tag (= :forms))
       [s cur]
       ;; not at the end so this is much simpler ... except for the cursor placement logic
       :default
       [(-> loc
            pe/barf-forward
            z/root-string)
        (cond
          ;; if it's the only one then point to one past the start of the vector/list
          (and (z/rightmost? loc) (z/leftmost? loc))
          (str-find-cursor s node-pos)
          ;; if it's at the right move the cursor to one past the left sibling
          (-> loc z/skip-whitespace z/rightmost?)
          (-> loc z/skip-whitespace-left z/node meta
              (#(str-find-cursor s % :at-end)))
          ;; just keep the same cursor
          :default cur)]))))

(defn slurp-backward
  "For a Buffer, slurp backward"
  ([] (slurp-backward j/*buffer*))
  ([buf]
   (let [cur (.cursor buf)
         s (str buf)
         old-tail-len (- (count s) cur)
         pos (str-find-pos s cur)
         loc (-> s
                 (z/of-string {:track-position? true})
                 (z/find-last-by-pos pos))
         new-s (if (coll-end? loc pos)
                 (-> (or (some-> loc z/down z/rightmost*)
                         (-> loc (z/insert-child (n/spaces 1)) z/down))
                     pe/slurp-backward
                     z/root-string)
                 (-> loc pe/slurp-backward z/root-string))]
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
         loc (-> s
                 (z/of-string {:track-position? true})
                 (z/find-last-by-pos pos))
         new-s (if (coll-end? loc pos)
                 (if-let [barfee (-> loc z/down)]
                   (->> barfee pe/barf-backward z/root-string)
                   s)
                 (->> loc pe/barf-backward z/root-string))]
     ;; TODO: do we need extra cursor management stuff like we do in barf forward?
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
  "splice the list/vector"
  ([] (splice j/*buffer*))
  ([buf]
   (let [[new-s new-cur] (splice (str buf) (.cursor buf))]
     (doto buf
       (.clear)
       (.write new-s)
       (.cursor new-cur))))
  ([s cur]
   (let [pos (str-find-pos s cur)
         loc (-> s
                 (z/of-string {:track-position? true})
                 (z/find-last-by-pos pos))]
     (if (and (z/sexpr-able? loc)
              (string? (z/sexpr loc))
              (> cur (->> loc z/node meta (str-find-cursor s))))
       ;; do special case of splicing a string
       (let [loc-pos (-> loc z/node meta)
             ;; TODO: check for balance/breaking splices and refuse with error message
             [s-beg s-end] (str-find-cursor-range s loc-pos)
             ;; TODO: Maybe we need spaces if no whitespace exists
             new-s (str (subs s 0 s-beg)  (subs s (inc s-beg) (dec s-end))  (subs s s-end))]
         [new-s (dec cur)])
       (let [new-s (-> loc
                       ((fn [loc]
                          (if (#{\} \) \]} (.charAt s cur))
                           loc
                           (z/up loc))))
                       (pe/splice)
                       (z/root-string))]
         [new-s cur])))))

(defn split-node [n c]
  (let [sexpr (n/sexpr n)
        is-string? (string? sexpr)
        s (cond-> sexpr (not is-string?) str)
        ;; if this is a string node the cursor input will be one too large
        ;; to accommodate the opening doublequote
        cur (if is-string? (dec c) c)]
    (cond->> [(subs s 0 cur) (subs s cur)]
             (not is-string?) (map edn/read-string)
             :then (map n/coerce))))

(defn split
  "split the list/vector"
  ([] (split j/*buffer*))
  ([buf]
   (let [s   (str buf)
         cur (.cursor buf)
         [new-s new-cur] (split s cur)]
     (doto buf
       (.clear)
       (.write new-s)
       (.cursor new-cur))))
  ([s cur]
   (let [pos (str-find-pos s cur)
         zloc (-> s
                  (z/of-string {:track-position? true})
                  (z/find-last-by-pos pos))
         loc-cursor  (str-find-cursor s (-> zloc z/node meta))
         inside-string? (and (> cur loc-cursor)
                             (z/sexpr-able? zloc)
                             (-> zloc z/sexpr string?))
         new-s (-> (cond
                     (coll-end? zloc pos) (z/insert-right zloc (-> zloc z/sexpr empty))
                     (z/whitespace? zloc) (pe/split-at-pos zloc pos) ; 3
                     ;; TODO: check to see if this logic is needed for inside-string?
                     ;;       rewrite-clj.paredit/split-at-pos claims to split strings OK
                     (> cur loc-cursor)
                     (let [[left-half right-half] (split-node (z/node zloc) (- cur loc-cursor))]
                       (-> zloc
                           (z/insert-left left-half)
                           (z/insert-right right-half)
                           (z/remove)
                           ;; pe/split will not work on top level type of :forms
                           (cond->
                             (not inside-string?) (pe/split))))
                     (z/leftmost? zloc) (z/insert-left (z/up zloc)  (-> zloc z/up z/sexpr empty)) ;2
                     (z/left zloc) (let [left-loc (z/left zloc)]
                                     (pe/split-at-pos left-loc (-> left-loc z/node meta)))
                     :default (throw (ex-info (str "could not split " (z/node zloc)) {}) ))
                   z/root-string)]
     ;; TODO: better cursor logic.  Maybe get it from the modified zloc above.
     [new-s (inc cur) #_(cond-> cur inside-string? inc)])))

(defn raise
  ([] (raise j/*buffer*))
  ([buf]
   (let [cur (.cursor buf)
         s   (str buf)
         pos (str-find-pos s cur)
         loc (-> s
                 (z/of-string {:track-position? true})
                 (z/find-last-by-pos pos))
         new-s (-> loc
                   (pe/raise)
                   (z/root-string))
         new-cur (str-find-cursor s (-> loc z/up z/node meta))]
     (doto buf
       (.clear)
       (.write new-s)
       (.cursor new-cur)))))

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
                   (z/find-last-by-pos cursor-pos))
           node-pos (-> (or (z/skip-whitespace loc) (z/up loc))
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
                         (select-keys orig-pos [:row :col]))
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


;; autopairing
(defn is-literal?
  [s c]
  (some->> (tokenize/tag-syntax s)
           (keep (fn [[_ beg end tag]]
                   (when (and (> c beg)
                              (< c end))
                     tag)))
           first
           #{:line-comment :string-literal}))

(defn open-round
  ([] (open-round j/*buffer*))
  ([buf] (let [s   (str buf)
               cur (.cursor buf)
               [new-s new-cur] (open-round s cur)]
           (doto buf
             (.clear)
             (.write new-s)
             (.cursor new-cur))))
  ([^String s ^Integer c]
   (if (is-literal? s c)
     [(str (subs s 0 c) "(" (subs s c)) (inc c)]
     (let [space-before (when (> c 0)
                          (-> (.charAt s (dec c))
                              (sexp/space-before)))
           space-after (when (< c (count s))
                         (-> (.charAt s c)
                             (sexp/space-after)))]
       [(str (subs s 0 c) space-before "()" space-after (subs s c))
        (+ c (if space-before 2 1))]))))

(defn open-square
  ;; FIXME: this is very wrong to not have generalized with open-round
  ([] (open-square j/*buffer*))
  ([buf] (let [s   (str buf)
               cur (.cursor buf)
               [new-s new-cur] (open-square s cur)]
           (doto buf
             (.clear)
             (.write new-s)
             (.cursor new-cur))))
  ([^String s ^Integer c]
   (if (is-literal? s c)
     [(str (subs s 0 c) "[" (subs s c)) (inc c)]
     (let [space-before (when (> c 0)
                          (-> (.charAt s (dec c))
                              (sexp/space-before)))
           space-after (when (< c (count s))
                         (-> (.charAt s c)
                             (sexp/space-after)))]
       [(str (subs s 0 c) space-before "[]" space-after (subs s c))
        (+ c (if space-before 2 1))]))))

(defn open-curly
  ;; FIXME: this is very wrong to not have generalized with open-round
  ([] (open-curly j/*buffer*))
  ([buf] (let [s   (str buf)
               cur (.cursor buf)
               [new-s new-cur] (open-curly s cur)]
           (doto buf
             (.clear)
             (.write new-s)
             (.cursor new-cur))))
  ([^String s ^Integer c]
   (if (is-literal? s c)
     [(str (subs s 0 c) "{" (subs s c)) (inc c)]
     (let [space-before (when (> c 0)
                          (-> (.charAt s (dec c))
                              (sexp/space-before)))
           space-after (when (< c (count s))
                         (-> (.charAt s c)
                             (sexp/space-after)))]
       [(str (subs s 0 c) space-before "{}" space-after (subs s c))
        (+ c (if space-before 2 1))]))))

(defn doublequote
  ;; a bit different from other autopairing as the open and close are bound to the same key
  ([] (doublequote j/*buffer*))
  ([buf] (let [s   (str buf)
               cur (.cursor buf)
               [new-s new-cur] (doublequote s cur)]
           (doto buf
             (.clear)
             (.write new-s)
             (.cursor new-cur))))
  ([^String s ^Integer c]
   (if (is-literal? s c)
     [(str (subs s 0 c) "\\\"" (subs s c)) (+ 2 c)]
     (let [space-before (when (> c 0)
                          (-> (.charAt s (dec c))
                              (sexp/space-before)))
           space-after (when (< c (count s))
                         (-> (.charAt s c)
                             (sexp/space-after)))]
       [(str (subs s 0 c) space-before "\"\"" space-after (subs s c))
        (+ c (if space-before 2 1))]))))

(defn close-round
  ([] (close-round j/*buffer*))
  ([buf] (if (is-literal? (str buf) (.cursor buf))
           (doto buf (.write (.getLastBinding j/*line-reader*)))
           (let [[_ _ end _] (some-> (tokenize/tag-sexp-traversal (str buf))
                                     (sexp/find-open-sexp-end (.cursor buf)))]
             (doto buf (.cursor end))))))

(defn backward-delete-char
  ([] (backward-delete-char j/*buffer*))
  ([buf] (condp #(%1 %2) (char (.prevChar buf))
           #{\) \] \}} (if (is-literal? (str buf) (.cursor buf))
                         (doto buf (.backspace))
                         (doto buf (.move -1)))
           {\( \)
            \[ \]
            \{ \}} :>> (fn [closer]
                                    (cond
                                      (is-literal? (str buf) (.cursor buf))
                                      (doto buf (.backspace))
                                      (= closer (char (.currChar buf)))
                                      (doto buf (.backspace) (.delete))
                                      :default buf))
           (doto buf (.backspace))))
  ([^String s ^Integer c]
   (let [buf (doto (BufferImpl.)
               (.write s)
               (.cursor c))]
     (backward-delete-char buf)
     [(str buf) (.cursor buf)])))

(defn delete-char
  ([] (delete-char j/*buffer*))
  ([buf] (condp #(%1 %2) (char (.currChar buf))
           #{\( \[ \{} (if (is-literal? (str buf) (.cursor buf))
                         (doto buf (.delete))
                         (doto buf (.move 1)))
           {\) \(
            \] \[
            \} \{} :>> (fn [opener]
                         (cond
                           (is-literal? (str buf) (.cursor buf))
                           (doto buf (.delete))
                           (= opener (char (.prevChar buf)))
                           (doto buf (.move -1) (.delete 2))
                           :default buf))
           (doto buf (.delete))))
  ([^String s ^Integer c]
   (let [buf (doto (BufferImpl.)
               (.write s)
               (.cursor c))]
     (delete-char buf)
     [(str buf) (.cursor buf)])))
