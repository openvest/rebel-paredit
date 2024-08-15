(ns rebel-readline.clojure.paredit-test
  (:require [rebel-readline.clojure.paredit :as SUT]
            [rebel-readline.jline-api :as j]
            [rebel-readline.core]
            [rewrite-clj.zip :as z]
            [clojure.string :as str]
            [clojure.test :refer :all])
  (:import [org.jline.reader.impl LineReaderImpl BufferImpl]
           [org.jline.terminal TerminalBuilder]))

;; items marked ^:wip are work in progress where non error returns are produced
;; with errors in cursor placement or whitespace
;; we may decide to handle some of that with rewrite-clj to do some reformatting

(defn str-cur
  [s+c]
  (let [cur (or (str/index-of s+c "|")
                (throw (ex-info "(with-buffer s ...) missing a | to indicate cursor" {:s s+c})))
        s (str (subs s+c 0 cur) (subs s+c (inc cur)))]
    [s cur]))

(defn display-str+cur
  "takes a buffer or [str cursor] and returns the string with a | where the cursor is"
  ([buf] (display-str+cur (str buf) (.cursor buf)))
  ([s cur]
   (str (subs s 0 cur) "|" (subs s cur))))

;; some helper functions/macros
(defmacro with-buffer
  "macro to run the body with jline-api/*buffer* bound to a buffer with the provided string
  Ths string must include a | to indicate the cursor position"
  [s & body]
  `(let [[s# cur#] (str-cur ~s)
         buffer# (doto (BufferImpl.)
                (.write s#)
                (.cursor cur#))
         #_#_line-reader# (rebel-readline.core/ensure-terminal (proxy [LineReaderImpl]
                              [j/*terminal*
                               "Test Readline"
                               (java.util.HashMap. {::j/service (atom {})})]))]
     (binding [j/*buffer* buffer#
               #_#_j/*line-reader* line-reader#]
       ~@body)))


;;;; String Only Tests
(def s1 "(defn f[x y]\n  (+ x 8))")

(deftest string-row-offsets-test-s1
  (is (= [0 13 24]
         (SUT/str-row-offsets s1))))

(deftest string-cursor-positions-s1
  (let [finder-fn (SUT/str-find-pos* s1)
        positions [{:col 1, :end-col 2, :end-row 1, :row 1}
                   {:col 2, :end-col 3, :end-row 1, :row 1}
                   {:col 3, :end-col 4, :end-row 1, :row 1}
                   {:col 4, :end-col 5, :end-row 1, :row 1}
                   {:col 5, :end-col 6, :end-row 1, :row 1}
                   {:col 6, :end-col 7, :end-row 1, :row 1}
                   {:col 7, :end-col 8, :end-row 1, :row 1}
                   {:col 8, :end-col 9, :end-row 1, :row 1}
                   {:col 9, :end-col 10, :end-row 1, :row 1}
                   {:col 10, :end-col 11, :end-row 1, :row 1}]]
    (is (fn? finder-fn))
    ;; TODO: extend these tests past 10
    (is (= positions
           (map finder-fn (range 10))))
    (is (= positions
           (map (partial SUT/str-find-pos s1) (range 10))))))

;;;; Zipper/Locator Tests
(def z1 (z/of-string s1 {:track-position? true}))

(deftest loc->position*-test
  (let [loc->position (SUT/loc->position* z1)]
    (is (fn? loc->position))
    (is (= {:col 1 :row 1 , :end-row 2, :end-col 11, :cursor 0,  :end-cursor 23}
           (loc->position z1)))
    (is (= {:col 8 :row 1 , :end-row 1, :end-col 13, :cursor 7,  :end-cursor 12} ; position of [x y]
           (loc->position (-> z1 z/next z/next z/next))))))

(deftest find-loc-test
  (is (= "defn"
         (-> (SUT/find-loc z1 1)
             z/node
             str)))
  (is (= "(+ x 8)"
         (-> (SUT/find-loc z1 15)
             z/node
             str)))
  (is (= s1
         (-> (SUT/find-loc z1 0)
             z/node
             str))))

;;;; Buffer Tests
;; kill tests

(deftest kill-foo-test
  (let [[beg-str beg-cur] (str-cur "(|foo bar)")
        [new-str new-cur] (str-cur "(|)")
        [end-str end-cur] (SUT/kill beg-str beg-cur)]
    (is (= new-str end-str))
    (is (= new-cur end-cur))))

(deftest kill-test
  (let [[beg-str beg-cur] (str-cur "(defn f[x y]\n  |(+ x 8))")
        [new-str new-cur] (str-cur "(defn f[x y]\n  |)")
        [end-str end-cur] (SUT/kill beg-str beg-cur)]
    (is (= new-str end-str))
    (is (= new-cur end-cur))))

(deftest kill-one-line-test
  "kill should kill upto but NOT including the <newline>"
  (let [[beg-str beg-cur] (str-cur "(defn f|[x y]\n  (if x\n    (+ x 8)\n    (+ y 3)))")
        [new-str new-cur] (str-cur "(defn f|\n  (if x\n    (+ x 8)\n    (+ y 3)))")
        [end-str end-cur] (SUT/kill beg-str beg-cur)]
    (is (= new-str end-str))
    (is (= new-cur end-cur))))

(deftest kill-at-line-end-test
  "kill with cursor placed at the end of the line"
  (let [[beg-str beg-cur] (str-cur "(defn f[x y]|\n  (+ x 8))")
        [new-str new-cur] (str-cur "(defn f[x y]|  (+ x 8))")
        [end-str end-cur] (SUT/kill beg-str beg-cur)]
    (is (= new-str end-str))
    (is (= new-cur end-cur))))

(deftest kill-at-buffer-end-test
  "kill with cursor placed at the end of the buffer"
  (let [[beg-str beg-cur] (str-cur "(foo bar)|")
        [new-str new-cur] (str-cur "(foo bar)|")
        [end-str end-cur] (SUT/kill beg-str beg-cur)]
    (is (= new-str end-str))
    (is (= new-cur end-cur))))

(deftest kill-end-test
  "if we end on a closing bracket do nothing"
  (let [[beg-str beg-cur] (str-cur "(foo (bar|))")
        [new-str new-cur] (str-cur "(foo (bar|))")
        [end-str end-cur] (SUT/kill beg-str beg-cur)]
    (is (= new-str end-str))
    (is (= new-cur end-cur))))

(deftest kill-origin-test
  "if we end on opening it should not fail (i.e. during lookback for single-quote)"
  (let [[beg-str beg-cur] (str-cur "|(foo (bar))")
        [new-str new-cur] (str-cur "|")
        [end-str end-cur] (SUT/kill beg-str beg-cur)]
    (is (= new-str end-str))
    (is (= new-cur end-cur))))

(deftest ^:wip kill-space-string-testa
  "some kills with a space before the string"
  ; seems to fail with one space before the double-quote char
  (let [[beg-str beg-cur] (str-cur "(foo | \"bar\")")
        [new-str new-cur] (str-cur "(foo |)")
        [end-str end-cur] (SUT/kill beg-str beg-cur)]
    (is (= new-str end-str))
    (is (= new-cur end-cur))))

(deftest kill-space-string2-test
  "some kills with a space "
  ; seems to fail with one space before the double-quote char
  (let [[beg-str beg-cur] (str-cur "(|foo bar)")
        [new-str new-cur] (str-cur "(|)")
        [end-str end-cur] (SUT/kill beg-str beg-cur)]
    (is (= new-str end-str))
    (is (= new-cur end-cur))))

; TODO: this test requires a killRing inside a line-reader.  Can't (yet) create this in testing
#_(deftest ^:wip kill-require-in-buf-test
  "wierd case of error inside a require"
  (with-buffer
    #_>>>> "(require '|[rewrite-clj.paredit :as paredit])"
    (is (= "(require '|)"
           (-> (SUT/kill-in-buff)
               (display-str+cur))))))

(deftest kill-require-test
  "wierd case of error inside a require"
  ;; TODO: should we be removing the quote here?
  (let [[beg-str beg-cur] (str-cur "(require '|[rewrite-clj.paredit :as paredit])")
        [new-str new-cur] (str-cur "(require '|)")
        [end-str end-cur] (SUT/kill beg-str beg-cur)]
    (is (= new-str end-str))
    (is (= new-cur end-cur))))

(deftest kill-at-whitespace-test
  "kill at whitespace node"
  (let [[beg-str beg-cur] (str-cur "[1 2|   3]")
        [new-str new-cur] (str-cur "[1 2|]")
        [end-str end-cur] (SUT/kill beg-str beg-cur)]
    (is (= new-str end-str))
    (is (= new-cur end-cur))))

(deftest kill-inside-whitespace-test
  "kill inside a whitespace node"
  (let [[beg-str beg-cur] (str-cur "[1 2  |   3]")
        [new-str new-cur] (str-cur "[1 2  |]")
        [end-str end-cur] (SUT/kill beg-str beg-cur)]
    (is (= new-str end-str))
    (is (= new-cur end-cur))))

(deftest kill-sym-test
  "kill inside a sym"
  (let [[beg-str beg-cur] (str-cur "(foo my-|symbol)")
        [new-str new-cur] (str-cur "(foo my-|)")
        [end-str end-cur] (SUT/kill beg-str beg-cur)]
    (is (= new-str end-str))
    (is (= new-cur end-cur))))

;; slurp and barf tests

(deftest slurp-forward-test
  (with-buffer
    #_>>>> "[[1 |2] [3 4] 5]"
    (is (= "[[1 |2 [3 4]] 5]"
           (-> (SUT/slurp-forward)
               (display-str+cur))))))

(deftest slurp-forward-tail-test
  "slurp forward when at the end of a list type node
   (i.e. no locator there)"
  (with-buffer
      #_>>>> "[[1 2|] [3 4] 5]"
      (is (= "[[1 2| [3 4]] 5]"
             (-> (SUT/slurp-forward)
                 (display-str+cur))))))

(deftest barf-forward-test
  (with-buffer
    #_>>>> "[[1 2| [3 4]] 5]"
    (is (= "[[1 2|] [3 4] 5]"
           (-> (SUT/barf-forward)
               (display-str+cur))))))

(deftest slurp-backward-test
  (with-buffer
    #_>>>> "[[1 2] [|3 4] 5]"
    (is (= "[[[1 2] |3 4] 5]"
           (-> (SUT/slurp-backward)
               (display-str+cur))))))

(deftest barf-backward-test
  (with-buffer
    #_>>>> "[[[1 2]| 3 4] 5]"
    (is (= "[[1 2] |[3 4] 5]"
           (-> (SUT/barf-backward)
               (display-str+cur))))))

;; splice and split tests

(deftest splice-test
  (with-buffer
    #_>>>> "[[1 2|] 3]"
    (is (= "[1 2 |3]"
           (-> (SUT/splice)
               (display-str+cur))))))

(deftest ^:cursor-pos splice-cursor-test
  ;; splice happens but cursor is misplaced
  (with-buffer
    #_>>>> "[1 2 [3 |4 5]]"
    (is (= "[1 2 3 |4 5]"
           (-> (rebel-readline.clojure.paredit/splice)
               (display-str+cur))))))

(deftest ^:wip splice-in-string-test
  ;; not sure if this is proper
  ;; this is the emacs and cursive behavior but could cause imbalanced parens
  ;; so maybe it is not a good thing??
  (with-buffer
    #_>>>> "(\"|foo bar\" x)"
    (is (= "(|foo bar x)"
           (-> (SUT/splice)
               (display-str+cur))))))

(deftest splice-at-tail-test
  (with-buffer
    #_>>>> "[[1 2|] 3]"
    (is (= "[1 2 |3]"
           (-> (SUT/splice)
               (display-str+cur))))))

(deftest ^:cursor-pos split-test
  ;; split happens but cursor is misplaced
  (with-buffer
    #_>>>> "[[1| 2] 3]"
    (is (= "[[1]| [2] 3]"
           (-> (SUT/split)
               (display-str+cur))))))

(deftest ^:wip split-not-ok-test
  ;; split happens but cursor is misplaced
  "this looks nearly identical to the above test
  it is still before the (node-2) but it fails"
  (with-buffer
    #_>>>> "[[1 |2] 3]"
    (is (= "[[1]| [2] 3]"
           (-> (SUT/split)
               (display-str+cur))))))

(deftest ^:cursor-pos split-at-string-test
  ;; split happens but cursor is misplaced
  (with-buffer
    #_>>>> "[[1 \"some-|long-string\"] 3]"
    (is (= "[[1 \"some-\"| \"long-string\"] 3]"
           (-> (SUT/split)
               (display-str+cur))))))

;; movement tests

(deftest ^:movement forward-1-test
  (with-buffer
    #_>>>> "[0 [1| :foo  3] :bar]"
    (is (= "[0 [1 :foo|  3] :bar]"
           (-> (SUT/forward)
               (display-str+cur))))))

(deftest ^:movement forward-2-test
  (with-buffer
    #_>>>> "[0 [1 :f|oo  3] :bar]"
    (is (= "[0 [1 :foo|  3] :bar]"
           (-> (SUT/forward)
               (display-str+cur))))))

(deftest ^:wip-movement forward-newline-test
  (with-buffer
    #_>>>> "[x|\n]"
    (is (= "[x\n]|"
           (-> (SUT/forward)
               (display-str+cur))))))

(deftest ^:movement forward-end-test
  (with-buffer
    #_>>>> "[x]|"
    (is (= "[x]|"
           (-> (SUT/forward)
               (display-str+cur))))))

(deftest ^:movement forward-multi-23-test
  (doall (for [[orig target] [["|[1 [22 :foo  bar]\n :z]"
                         "[1 [22 :foo  bar]\n :z]|"]

                        ["[|1 [22 :foo  bar]\n :z]"
                         "[1| [22 :foo  bar]\n :z]"]

                        ["[1| [22 :foo  bar]\n :z]"
                         "[1 [22 :foo  bar]|\n :z]"]

                        ["[1 |[22 :foo  bar]\n :z]"
                         "[1 [22 :foo  bar]|\n :z]"]

                        ["[1 [|22 :foo  bar]\n :z]"
                         "[1 [22| :foo  bar]\n :z]"]

                        ["[1 [2|2 :foo  bar]\n :z]"
                         "[1 [22| :foo  bar]\n :z]"]

                        ["[1 [22| :foo  bar]\n :z]"
                         "[1 [22 :foo|  bar]\n :z]"]

                        ["[1 [22 |:foo  bar]\n :z]"
                         "[1 [22 :foo|  bar]\n :z]"]

                        ["[1 [22 :|foo  bar]\n :z]"
                         "[1 [22 :foo|  bar]\n :z]"]

                        ["[1 [22 :f|oo  bar]\n :z]"
                         "[1 [22 :foo|  bar]\n :z]"]

                        ["[1 [22 :fo|o  bar]\n :z]"
                         "[1 [22 :foo|  bar]\n :z]"]

                        ["[1 [22 :foo|  bar]\n :z]"
                         "[1 [22 :foo  bar|]\n :z]"]

                        ["[1 [22 :foo | bar]\n :z]"
                         "[1 [22 :foo  bar|]\n :z]"]

                        ["[1 [22 :foo  |bar]\n :z]"
                         "[1 [22 :foo  bar|]\n :z]"]

                        ["[1 [22 :foo  b|ar]\n :z]"
                         "[1 [22 :foo  bar|]\n :z]"]

                        ["[1 [22 :foo  ba|r]\n :z]"
                         "[1 [22 :foo  bar|]\n :z]"]

                        ["[1 [22 :foo  bar|]\n :z]"
                         "[1 [22 :foo  bar]|\n :z]"]

                        ["[1 [22 :foo  bar]|\n :z]"
                         "[1 [22 :foo  bar]\n :z|]"]

                        ["[1 [22 :foo  bar]\n| :z]"
                         "[1 [22 :foo  bar]\n :z|]"]

                        ["[1 [22 :foo  bar]\n |:z]"
                         "[1 [22 :foo  bar]\n :z|]"]

                        ["[1 [22 :foo  bar]\n :|z]"
                         "[1 [22 :foo  bar]\n :z|]"]

                        ["[1 [22 :foo  bar]\n :z|]"
                         "[1 [22 :foo  bar]\n :z]|"]

                        ["[1 [22 :foo  bar]\n :z]|"
                         "[1 [22 :foo  bar]\n :z]|"]]
         :let [[s cur] (str-cur orig)]]
           (do
             (is (= target
                    (->> (SUT/forward s cur)
                         (display-str+cur s))))))))

(deftest ^:movement backward-1-test
  (with-buffer
    #_>>>> "[0 [1| :foo  3] :bar]"
    (is (= "[0 [|1 :foo  3] :bar]"
           (-> (SUT/backward)
               (display-str+cur))))))

(deftest ^:movement backward-2-test
  (with-buffer
    #_>>>> "[0 [1 :f|oo  3] :bar]"
    (is (= "[0 [1 |:foo  3] :bar]"
           (-> (SUT/backward)
               (display-str+cur))))))

(deftest ^:movement backward-multi-23-test
  (doall (for [[orig target] [["|[1 [22 :foo  bar]\n:z]"
                               "|[1 [22 :foo  bar]\n:z]"]
                              ["[|1 [22 :foo  bar]\n:z]"
                               "|[1 [22 :foo  bar]\n:z]"]
                              ["[1| [22 :foo  bar]\n:z]"
                               "[|1 [22 :foo  bar]\n:z]"]
                              ["[1 |[22 :foo  bar]\n:z]"
                               "[|1 [22 :foo  bar]\n:z]"]
                              ["[1 [|22 :foo  bar]\n:z]"
                               "[1 |[22 :foo  bar]\n:z]"]
                              ["[1 [2|2 :foo  bar]\n:z]"
                               "[1 [|22 :foo  bar]\n:z]"]
                              ["[1 [22| :foo  bar]\n:z]"
                               "[1 [|22 :foo  bar]\n:z]"]
                              ["[1 [22 |:foo  bar]\n:z]"
                               "[1 [|22 :foo  bar]\n:z]"]
                              ["[1 [22 :|foo  bar]\n:z]"
                               "[1 [22 |:foo  bar]\n:z]"]
                              ["[1 [22 :f|oo  bar]\n:z]"
                               "[1 [22 |:foo  bar]\n:z]"]
                              ["[1 [22 :fo|o  bar]\n:z]"
                               "[1 [22 |:foo  bar]\n:z]"]
                              ["[1 [22 :foo|  bar]\n:z]"
                               "[1 [22 |:foo  bar]\n:z]"]
                              ["[1 [22 :foo | bar]\n:z]"
                               "[1 [22 |:foo  bar]\n:z]"]
                              ["[1 [22 :foo  |bar]\n:z]"
                               "[1 [22 |:foo  bar]\n:z]"]
                              ["[1 [22 :foo  b|ar]\n:z]"
                               "[1 [22 :foo  |bar]\n:z]"]
                              ["[1 [22 :foo  ba|r]\n:z]"
                               "[1 [22 :foo  |bar]\n:z]"]
                              ;; end of vector
                              ["[1 [22 :foo  bar|]\n:z]"
                               "[1 [22 :foo  |bar]\n:z]"]
                              ["[1 [22 :foo  bar]|\n:z]"
                               "[1 |[22 :foo  bar]\n:z]"]
                              ["[1 [22 :foo  bar]\n|:z]"
                               "[1 |[22 :foo  bar]\n:z]"]
                              ["[1 [22 :foo  bar]\n|:z]"
                               "[1 |[22 :foo  bar]\n:z]"]
                              ["[1 [22 :foo  bar]\n:|z]"
                               "[1 [22 :foo  bar]\n|:z]"]
                              ;; end of vector
                              ["[1 [22 :foo  bar]\n:z|]"
                               "[1 [22 :foo  bar]\n|:z]"]
                              ["[1 [22 :foo  bar]\n:z]|"
                               "|[1 [22 :foo  bar]\n:z]"]]
               :let [[s cur] (str-cur orig)]]
           (do
             (is (= target
                    (try (->> (SUT/backward s cur)
                              (display-str+cur s))
                         (catch Exception e  (str "ERROR on backward movement of " orig)))))))))

(comment
  ;; look for error if we end with whitespace like "[x\n]"
  (let [s "[1 [22 :foo  bar]\n :z]"
        z (z/of-string s {:track-position? true})]
    (for [cur (range (count s))
          :let [cursor-pos (SUT/str-find-pos s cur)
                node-pos (-> (z/find-last-by-pos z cursor-pos)
                             (z/skip-whitespace)
                             (z/node)
                             meta)
                orig-s (display-str+cur s cur)
                new-s (try (->> (SUT/backward s cur)
                                (display-str+cur s))
                           (catch Exception e "Error"))]]
      [#_#_cursor-pos node-pos orig-s new-s ])))
(comment
  ;all cursor positions
  (let [buf (doto (BufferImpl.)
              (.write "[[1 2] 3]"))
        s (str buf)]
    (for [cur (range (inc (count s)))]
      (str (doto buf
             (.clear)
             (.write s)
             (.cursor cur)
             (.write "|")))))
)

