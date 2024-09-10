(ns repl-balance.clojure.paredit-test
  (:require [repl-balance.clojure.paredit :as SUT]
            [repl-balance.jline-api :as j]
            [repl-balance.core]
            [rewrite-clj.zip :as z]
            [clojure.string :as str]
            [clojure.test :refer :all]
            [repl-balance.test-helpers :refer [split-s-cur join-s-cur s-cur-test reader-test]])
  (:import [org.jline.reader.impl LineReaderImpl BufferImpl]))

;; items marked ^:wip are work in progress where non error returns are produced
;; with errors in cursor placement or whitespace
;; we may decide to handle some of that with rewrite-clj to do some reformatting

;; some helper functions/macros
(defmacro with-buffer
  "macro to run the body with jline-api/*buffer* bound to a buffer with the provided string
  Ths string must include a | to indicate the cursor position"
  [s & body]
  `(let [[s# cur#] (split-s-cur ~s)
         buffer# (doto (BufferImpl.)
                   (.write ^String s#)
                   (.cursor ^Integer cur#))
         #_#_line-reader# (repl-balance.core/ensure-terminal (proxy [LineReaderImpl]
                              [j/*terminal*
                               "Test Readline"
                               (java.util.HashMap. {::j/service (atom {})})]))]
     (binding [j/*buffer* buffer#
               #_#_j/*line-reader* line-reader#]
       ~@body)))


;;;; String Only Tests
(def s1 "(defn f[x y]\n  (+ x 8))")

(deftest str-row-offsets-test-s1
  (is (= [0 13 24]
         (SUT/str-row-offsets s1))))

(deftest str-find-pos*-s1-test
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
  (let [[beg-str beg-cur] (split-s-cur "(|foo bar)")
        [new-str new-cur] (split-s-cur "(|)")
        [end-str end-cur] (SUT/kill beg-str beg-cur)]
    (is (= new-str end-str))
    (is (= new-cur end-cur))))

(deftest kill-test
  (let [[beg-str beg-cur] (split-s-cur "(defn f[x y]\n  |(+ x 8))")
        [new-str new-cur] (split-s-cur "(defn f[x y]\n  |)")
        [end-str end-cur] (SUT/kill beg-str beg-cur)]
    (is (= new-str end-str))
    (is (= new-cur end-cur))))

(deftest kill-one-line-test
  "kill should kill upto but NOT including the <newline>"
  (let [[beg-str beg-cur] (split-s-cur "(defn f|[x y]\n  (if x\n    (+ x 8)\n    (+ y 3)))")
        [new-str new-cur] (split-s-cur "(defn f|\n  (if x\n    (+ x 8)\n    (+ y 3)))")
        [end-str end-cur] (SUT/kill beg-str beg-cur)]
    (is (= new-str end-str))
    (is (= new-cur end-cur))))

(deftest kill-at-line-end-test
  "kill with cursor placed at the end of the line"
  (let [[beg-str beg-cur] (split-s-cur "(defn f[x y]|\n  (+ x 8))")
        [new-str new-cur] (split-s-cur "(defn f[x y]|(+ x 8))")
        [end-str end-cur] (SUT/kill beg-str beg-cur)]
    (is (= new-str end-str))
    (is (= new-cur end-cur))))

(deftest kill-at-buffer-end-test
  "kill with cursor placed at the end of the buffer"
  (let [[beg-str beg-cur] (split-s-cur "(foo bar)|")
        [new-str new-cur] (split-s-cur "(foo bar)|")
        [end-str end-cur] (SUT/kill beg-str beg-cur)]
    (is (= new-str end-str))
    (is (= new-cur end-cur))))

(deftest kill-end-test
  "if we end on a closing bracket do nothing"
  (let [[beg-str beg-cur] (split-s-cur "(foo (bar|))")
        [new-str new-cur] (split-s-cur "(foo (bar|))")
        [end-str end-cur] (SUT/kill beg-str beg-cur)]
    (is (= new-str end-str))
    (is (= new-cur end-cur))))

(deftest kill-origin-test
  "if we end on opening it should not fail (i.e. during lookback for single-quote)"
  (let [[beg-str beg-cur] (split-s-cur "|(foo (bar))")
        [new-str new-cur] (split-s-cur "|")
        [end-str end-cur] (SUT/kill beg-str beg-cur)]
    (is (= new-str end-str))
    (is (= new-cur end-cur))))

(deftest ^:whitespace kill-space-string-test
  "some kills with a space before the string"
  ; seems to fail with one space before the doublequote char
  (let [[beg-str beg-cur] (split-s-cur "(foo | \"bar\")")
        [new-str new-cur] (split-s-cur "(foo |)")
        [end-str end-cur] (SUT/kill beg-str beg-cur)]
    (is (= new-str end-str))
    (is (= new-cur end-cur))))

(deftest kill-space-string-test2
  "some kills without space before the string"
  (s-cur-test SUT/kill
              "(foo |\"bar\")"
              "(foo |)"))

(deftest kill-open-brace-in-kill-test
  "kill should include the element that starts between the cursor and line end"
  (s-cur-test SUT/kill
              "(let |[foo 1\n   bar 2])"
              "(let |)"))

(deftest kill-end-of-root-test
  "some kills without space before the string"
  (s-cur-test SUT/kill
              "(|)"
              "(|)"))

(deftest kill-space-string-test3
  (s-cur-test SUT/kill
              "(fo|o \"bar\")"
              "(fo|)"))

(deftest kill-space-string2-test
  "some kills with a space "
  ; seems to fail with one space before the doublequote char
  (let [[beg-str beg-cur] (split-s-cur "(|foo bar)")
        [new-str new-cur] (split-s-cur "(|)")
        [end-str end-cur] (SUT/kill beg-str beg-cur)]
    (is (= new-str end-str))
    (is (= new-cur end-cur))))

(deftest kill-hash-mark-test
  (let [[beg-str beg-cur] (split-s-cur "(|#(inc %) 2)")
        [new-str new-cur] (split-s-cur "(|)")
        [end-str end-cur] (SUT/kill beg-str beg-cur)]
    (is (= new-str end-str))
    (is (= new-cur end-cur))))

(deftest kill-invalid-symbol-tests
  "This can arise from (x/s1|\nx/s2) and kill twice"
  ;; TODO: check this as the test passes but fails in repl
  (let [[beg-str beg-cur] (split-s-cur "(x/symbol1|z/symbol2)")
        [new-str new-cur] (split-s-cur "(x/symbol1|)")
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
               (join-s-cur))))))

(deftest kill-require-test
  "wierd case of error inside a require"
  ;; TODO: should we be removing the quote here?
  (let [[beg-str beg-cur] (split-s-cur "(require '|[rewrite-clj.paredit :as paredit])")
        [new-str new-cur] (split-s-cur "(require '|)")
        [end-str end-cur] (SUT/kill beg-str beg-cur)]
    (is (= new-str end-str))
    (is (= new-cur end-cur))))

(deftest kill-at-whitespace-test
  "kill at whitespace node"
  (let [[beg-str beg-cur] (split-s-cur "[1 2|   3]")
        [new-str new-cur] (split-s-cur "[1 2|]")
        [end-str end-cur] (SUT/kill beg-str beg-cur)]
    (is (= new-str end-str))
    (is (= new-cur end-cur))))

(deftest ^:whitespace kill-inside-whitespace-test
  "kill inside a whitespace node"
  (let [[beg-str beg-cur] (split-s-cur "[1 2  |   3]")
        [new-str new-cur] (split-s-cur "[1 2  |]")
        [end-str end-cur] (SUT/kill beg-str beg-cur)]
    (is (= new-str end-str))
    (is (= new-cur end-cur))))

(deftest kill-inside-string-literal-test
  "kill inside a string"
  ;; TODO: is this different with buffer test vs string+cursor test
  (s-cur-test SUT/kill
              "[:a \"some| string\"]"
              "[:a \"some|\"]"))

(deftest kill-short-empty-test
  (s-cur-test SUT/kill
              "(|)"
              "(|)"))

(deftest kill-sym-test
  "kill inside a sym"
  (let [[beg-str beg-cur] (split-s-cur "(foo my-|symbol)")
        [new-str new-cur] (split-s-cur "(foo my-|)")
        [end-str end-cur] (SUT/kill beg-str beg-cur)]
    (is (= new-str end-str))
    (is (= new-cur end-cur))))

(deftest ^:balance kill-multiline-test
  (let [[beg-str beg-cur] (split-s-cur "[|(and (or x\n y)\n z)]")
        [new-str new-cur] (split-s-cur "[|]")
        [end-str end-cur] (SUT/kill beg-str beg-cur)]
    (is (= new-str end-str))
    (is (= new-cur end-cur))))

(deftest kill-literal-end-double-doublequote-test
  "An escaped doublequote at the end of a literal"
  (let [[beg-str beg-cur] (split-s-cur "\"foo\\\"|\"")
        [new-str new-cur] (split-s-cur "\"foo|\"")
        [end-str end-cur] (SUT/kill beg-str beg-cur)]
    (is (= new-str end-str))
    (is (= new-cur end-cur))))

;; slurp and barf tests

(deftest slurp-forward-test
  (with-buffer
    #_>>>> "[[1 |2] [3 4] 5]"
    (is (= "[[1 |2 [3 4]] 5]"
           (-> (SUT/slurp-forward)
               (join-s-cur))))))

(deftest slurp-forward-tail-test
  "slurp forward when at the end of a list type node
   (i.e. no locator there)"
  (with-buffer
      #_>>>> "[[1 2|] [3 4] 5]"
      (is (= "[[1 2| [3 4]] 5]"
             (-> (SUT/slurp-forward)
                 (join-s-cur))))))

(deftest slurp-forward-multiline-align-test
  "slurp forward where auto-align gets things wrong"
  (with-buffer
    #_>>>> "[foo bar\n      baz|] (and a b)"
    (is (= "[foo bar\n baz (and a b)]"
           (-> (SUT/slurp-forward)
               (join-s-cur))))))

(deftest ^:balance slurp-forward-reindent-test
  "slurping included a reindent which throws off the tail
  so if we reindent we can't just cut a section out form the cursor any longer"
  (with-buffer
    #_>>>> "(let [a 1\n      b|]\n  :ovx)"
    (is (= "(let [a 1\n      b|\n  :ovx])"
           (-> (SUT/slurp-forward)
               (join-s-cur))))))

;; slurp and barf within a quoted syntax string

(deftest slurp-forward-quoted-syntax-test
  "slurp forward when there is a quote i.e. invalid sexp"
  (with-buffer
    #_>>>> "(require '[|]this)"
    (is (= "\"(require '[|this])"
           (-> (SUT/slurp-forward)
               (join-s-cur)))))
  (with-buffer
    #_>>>> "@(|def x) 3"
    (is (= "@(|def x 3)"
           (-> (SUT/slurp-forward)
               (join-s-cur))))))

(deftest barf-forward-quoted-syntax-test
  "barf forward when there is a quote i.e. invalid sexp"
  (with-buffer
    #_>>>> "[:a `[|:z]]"
    (is (= "[:a `[|] :z]"
           (-> (SUT/barf-forward)
               (join-s-cur))))))

(deftest slurp-backward-quoted-syntax-test
  "slurp backward when there is a quote i.e. invalid sexp"
  (with-buffer
    #_>>>> "[apply `(vec |:z)]"
    (is (= "[`(apply vec |:z)]" ;; emacs "[(apply `vec |:z)]"
           (-> (SUT/slurp-backward)
               (join-s-cur))))))


(deftest barf-backward-quoted-syntax-test
  "barf forward when there is a quote i.e. invalid sexp"
  (with-buffer
    #_>>>> "[:a `[:b| :c]]"
    (is (= "[:a :b `[:c]"  ;; emacs "[:a `:b [:c]]"
           (-> (SUT/barf-backward)
               (join-s-cur))))))


(deftest barf-forward-test
  (with-buffer
    #_>>>> "[[1 2| [3 4]] 5]"
    (is (= "[[1 2|] [3 4] 5]"
           (-> (SUT/barf-forward)
               (join-s-cur))))))

(deftest barf-forward-at-last-node-test
  "rewrite-clj compresses spaces
  Note: emacs and intellij have different behavior
  emacs keeps the cursor in place which it is then outside
  cursive keeps the cursor inside the vector and compresses extra spaces
  this also makes cursive able to barf multiple times by repeating"
  ; TODO: this causes an invalid sexp
  (testing "barf-forward"
    (with-buffer
      #_>>>> "[1 |123]"
      (is (= "[1|] 123"
             (-> (SUT/barf-forward)
                 (join-s-cur)))))
    (with-buffer
      #_>>>> "[1 123|]"
      (is (= "[1|] 123"
             (-> (SUT/barf-forward)
                 (join-s-cur)))))
    (with-buffer
      #_>>>> "[|]"
      (is (= "[|]"
             (-> (SUT/barf-forward)
                 (join-s-cur)))))
    (with-buffer
      #_>>>> "[[1   |  123]]"
      (is (= "[[1|] 123]"
             (-> (SUT/barf-forward)
                 (join-s-cur)))))
    ))

(deftest ^:cursor-pos barf-forward-str-node-test
  "rewrite-clj compresses spaces
  Note: emacs and intellij have different behavior"
  (testing "barf-forward-str from : [[1  |   123]]"
    (let [[s cur]
          (split-s-cur
             "[[1   |  123]]")]
      (is (= (->> (SUT/barf-forward-str s cur)
                  (apply join-s-cur))
             "[[1|] 123]")))))

(deftest barf-forward-str-repl-test
  "does not test 'correctness' only that no error is throw.
  But it does it for every cursor position!"
  (let [s "[1 [:a :b :see] :z]"]
    (doall (for  [c (range (count s))]
             (let [orig (join-s-cur s c)
                   modified (try
                              (->> (SUT/barf-forward-str s c)
                                   (apply join-s-cur))
                              :no-error
                              (catch Exception e (->> e Throwable->map :cause)))]
               (testing (str "barf-forward-str with: "orig)
                 (is (= :no-error modified))))))))

(deftest barf-forward-edge-case1-test
  "wierd edge-case here barf fails
  required a newer version of rewrite-clj than is specified by cljfmt"
  (with-buffer
    #_>>>> "[1 |[3] 4]"
    (is (= "[1 |[3]] 4"
           (-> (SUT/barf-forward)
               (join-s-cur))))))

(deftest slurp-backward-test
  (with-buffer
    #_>>>> "[[1 2] [|3 4] 5]"
    (is (= "[[[1 2] |3 4] 5]"
           (-> (SUT/slurp-backward)
               (join-s-cur))))))

(deftest slurp-backward-from-last-element-test
  (with-buffer
    #_>>>> "[1 [2 3|]]"
    (is (= "[[1 2 3|]]"
           (-> (SUT/slurp-backward)
               (join-s-cur))))))

(deftest barf-backward-test
  (with-buffer
    #_>>>> "[[[1 2]| 3 4] 5]"
    (is (= "[[1 2] |[3 4] 5]"
           (-> (SUT/barf-backward)
               (join-s-cur))))))

(deftest barf-backward-from-last-element-test
  (with-buffer
    #_>>>> "[1 [2 3|]]"
    (is (= "[1 2 [3|]]"
           (-> (SUT/barf-backward)
               (join-s-cur))))))

(deftest slurp-backward-from-empty-coll-test
    (with-buffer
      #_>>>> "[1 [|] 2]"
      (is (= "[[1|] 2]"
             (-> (SUT/slurp-backward)
                 (join-s-cur))))))

;; splice and split tests

(deftest splice-test
  (with-buffer
    #_>>>> "[[1 2|] 3]"
    (is (= "[1 2 |3]"
           (-> (SUT/splice)
               (join-s-cur))))))

(deftest ^:cursor-pos splice-cursor-test
  ;; splice happens but cursor is misplaced
  (with-buffer
    #_>>>> "[1 2 [3 |4 5]]"
    (is (= "[1 2 3 |4 5]"
           (-> (SUT/splice)
               (join-s-cur))))))

(deftest ^:wip splice-in-string-test
  ;; not sure if this is proper
  ;; this is the emacs and cursive behavior but could cause imbalanced parens
  ;; so, maybe it is not a good thing??
  ;; maybe allow if balanced, else reject
  (with-buffer
    #_>>>> "(\"|foo bar\" x)"
    (is (= "(|foo bar x)"
           (-> (SUT/splice)
               (join-s-cur))))))

(deftest ^:cursor-pos splice-at-tail-test
  (with-buffer
    #_>>>> "[[1 2|] 3]"
    (is (= "[1 2 |3]"
           (-> (SUT/splice)
               (join-s-cur))))))

(deftest split-test
  ;; split happens but cursor is misplaced
  (with-buffer
    #_>>>> "[[1| 2] 3]"
    (is (= "[[1]| [2] 3]"
           (-> (SUT/split)
               (join-s-cur))))))

(deftest ^:cursor-pos split-at-last-node-test
  "this looks nearly identical to the above test
  it is still before the (node-2) but it fails"
  ;; note that rewrite-clj.paredit/split expects the loc to be to the left to the split target
  (with-buffer
    #_>>>> "[[1 |2] 3]"
    (is (= "[[1]| [2] 3]"
           (-> (SUT/split)
               (join-s-cur))))))


(deftest ^:cursor-pos split-at-coll-end-test
  (with-buffer
    #_>>>> "[[1 2|] 3]"
    (is (= "[[1 2]| [] 3]"
           (-> (SUT/split)
               (join-s-cur))))))

(deftest split-everywhere-14-tests
  (let [correct-splits [;; cannot insert at top error
                        #_["|[[1 22 33] 4]"
                           "|[[1 22 33] 4]"]

                        ["[|[1 22 33] 4]"
                         "[]| [[1 22 33] 4]"]

                        ["[[|1 22 33] 4]"
                         "[[]| [1 22 33] 4]"]

                        ["[[1| 22 33] 4]"
                         "[[1]| [22 33] 4]"]

                        ["[[1 |22 33] 4]"
                         "[[1]| [22 33] 4]"]

                        ["[[1 2|2 33] 4]"
                         "[[1 2]| [2 33] 4]"]

                        ["[[1 22| 33] 4]"
                         "[[1 22]| [33] 4]"]

                        ["[[1 22 |33] 4]"
                         "[[1 22]| [33] 4]"]

                        ["[[1 22 3|3] 4]"
                         "[[1 22 3]| [3] 4]"]

                        ["[[1 22 33|] 4]"
                         "[[1 22 33]| [] 4]"]

                        ["[[1 22 33]| 4]"
                         "[[1 22 33]]| [4]"]

                        ["[[1 22 33] |4]"
                         "[[1 22 33]]| [4]"]

                        ["[[1 22 33] 4|]"
                         "[[1 22 33] 4]| []"]
                        ;; emacs throws an error and intellij does nothing
                        #_["[[1 22 33] 4]|"
                         "[[1 22 33] 4]| []"]]]
    (dorun (for [[orig target] correct-splits
                 :let [[s cur] (split-s-cur orig)
                       [target-s target-cur] (split-s-cur target)
                       [new-s new-cur] (SUT/split s cur)]]
             (testing (str "split: " orig)
               (is (= target-s new-s))
               ;; TODO: get cursors right but for now just testing the new-string(s)
               #_(is (= target
                      (join-s-cur new-s new-cur))))))))

(deftest split-at-string-test
  ;; split happens but cursor is misplaced
  (with-buffer
    #_>>>> "[[1 \"some-|long-string\"] 3]"
    (is (= "[[1 \"some-\"| \"long-string\"] 3]"
           (-> (SUT/split)
               (join-s-cur))))))

;; movement tests

(deftest ^:movement forward-1-test
  (with-buffer
    #_>>>> "[0 [1| :foo  3] :bar]"
    (is (= "[0 [1 :foo|  3] :bar]"
           (-> (SUT/forward)
               (join-s-cur))))))

(deftest ^:movement forward-2-test
  (with-buffer
    #_>>>> "[0 [1 :f|oo  3] :bar]"
    (is (= "[0 [1 :foo|  3] :bar]"
           (-> (SUT/forward)
               (join-s-cur))))))

(deftest ^:movement skip-doublequote-test
  (with-buffer
    #_>>>> "(\"foo|\")"
    (is (= "(\"foo\"|)"
           (-> (SUT/forward)
               (join-s-cur))))))

(def SUT (-> (ns-aliases *ns*)
             (get 'SUT)
             (str)
             (str/replace "-" "_")))

;; This is long, but it's my first attempt at a "reduced size" exception report
;; the report is returned as a Fail not an Error
;; if this is worthwhile, it should be baked into a macro
;; assumes the SUT is the module being tested
;; look at the test reporting facilities in clojure or kaocha as a better alternative
(deftest forward-newline-test
  (with-buffer
    #_>>>> "[x|\n]"
    (is (= "[x\n]|"
           (try
             (->> (SUT/forward "[x\n]" 2)
                  (join-s-cur  "[x\n]"))
             (catch Exception e (-> e
                                    Throwable->map
                                    (update :trace
                                            (partial filter #(str/starts-with? (str (first %))  SUT)))
                                    (assoc-in [:data :SUT] SUT))))))))

(deftest ^:movement forward-end-test
  (with-buffer
    #_>>>> "[x]|"
    (is (= "[x]|"
           (-> (SUT/forward)
               (join-s-cur))))))

(deftest ^:movement forward-multi-23-test
  (dorun (for [[orig target] [["|[1 [22 :foo  bar]\n :z]"
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
         :let [[s cur] (split-s-cur orig)]]
           (is (= target
                  (->> (SUT/forward s cur)
                       (join-s-cur s)))))))

(deftest ^:movement backward-1-test
  (with-buffer
    #_>>>> "[0 [1| :foo  3] :bar]"
    (is (= "[0 [|1 :foo  3] :bar]"
           (-> (SUT/backward)
               (join-s-cur))))))

(deftest ^:movement backward-2-test
  (with-buffer
    #_>>>> "[0 [1 :f|oo  3] :bar]"
    (is (= "[0 [1 |:foo  3] :bar]"
           (-> (SUT/backward)
               (join-s-cur))))))

(deftest ^:movement backward-multi-23-test
  (dorun (for [[orig target] [["|[1 [22 :foo  bar]\n:z]"
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
               :let [[s cur] (split-s-cur orig)]]
           (testing (str "backward: " (with-out-str (pr orig)))
             (is (= target
                    (try (->> (SUT/backward s cur)
                              (join-s-cur s))
                         (catch Exception _e (str "ERROR on backward movement of " orig)))))))))

(deftest ^:cursor-pos backward-last-top-form-test
  "test for last top level form, should not go to buffer beginning"
  (with-buffer
    #_>>>> "(foo)(bar)|"
    (is (= "(foo)|(bar)"
           (-> (SUT/backward)
               (join-s-cur))))))

(deftest paredit-close-square-at-comment-end-of-line-test
  "this should behave as though it is still inside the literal"
  (reader-test SUT/close-round "]"
               "; Hello, world!|\n"
               "; Hello, world!]|\n"))

(deftest ^:balance paredit-comment-b4-opening-test
  "The key here is to not break paren balance"
  (s-cur-test SUT/line-comment
              "|(def x\n  3)"
              ";; |\n(def x\n  3)"))

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
                orig-s (join-s-cur s cur)
                new-s (try (->> (SUT/backward s cur)
                                (join-s-cur s))
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
