(ns rebel-readline.clojure.paredit-test
  (:require [rebel-readline.clojure.paredit :as SUT]
            [rebel-readline.jline-api :as j]
            [rewrite-clj.zip :as z]
            [clojure.string :as str]
            [clojure.test :refer :all])
  (:import [org.jline.reader.impl BufferImpl]))

;; items marked ^:wip are work in progress where non error returns are produced
;; with errors in cursor placement or whitespace
;; we may decide to handle some of that with rewrite-clj to do some reformatting

;; some helper functions/macros
(defmacro with-buffer
  "macro to run the body with jline-api/*buffer* bound to a buffer with the provided string
  Ths string must include a | to indicate the cursor position"
  [s & body]
  `(let [cur# (or (str/index-of ~s "|")
                  (throw (ex-info "(with-buffer s ...) missing a | to indicate cursor" {:s ~s})))
         s# (str (subs ~s 0 cur#) (subs ~s (inc cur#)))
         buf# (doto (BufferImpl.)
                (.write s#)
                (.cursor cur#))]
     (binding [j/*buffer* buf#]
       ~@body)))

(defn display-buffer [buf]
  "takes a buffer and returns the string with a | where the cursor is"
  (let [s (str buf)]
    (str (subs s 0 (.cursor buf)) "|"  (subs s (.cursor buf)))))

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

(deftest kill-test
  (with-buffer
    #_>>>> "(defn f[x y]\n  |(+ x 8))"
    (is (= "(defn f[x y]\n  |)"
           (-> (SUT/kill)
               (display-buffer))))))

(deftest kill-one-line-test
  "kill should kill upto but NOT including the <newline>"
  (with-buffer
    #_>>>> "(defn f|[x y]\n  (if x\n    (+ x 8)\n    (+ y 3)))"
    (is (= "(defn f|\n  (if x\n    (+ x 8)\n    (+ y 3)))"
           (-> (SUT/kill)
               (display-buffer))))))

(deftest ^:wip kill-at-line-end-test
  ;; split happens but cursor is misplaced
  "kill with cursor placed at the end of the line"
  (with-buffer
    #_>>>> "(defn f[x y]|\n  (+ x 8))"
    (is (= "(defn f[x y]| (+ x 8))"
           (-> (SUT/kill)
               (display-buffer))))))

(deftest kill-end-test
  ;; FIXME: this breaks the paren balance
  "if we end on a closing bracket do nothing"
  (with-buffer
    #_>>>> "(foo (bar|))"
    (is (= "(foo (bar|))"
           (-> (SUT/kill)
               (display-buffer))))))

(deftest kill-require-test
  "wierd case of error inside a require"
  ;; TODO: should we be removing the quote here?
  (with-buffer
    #_>>>> "(require '|[rewrite-clj.paredit :as paredit])"
    (is (= "(require |)"
           (-> (SUT/kill)
               (display-buffer))))))

(deftest kill-at-whitespace-test
  "kill at whitespace node"
  (with-buffer
    #_>>>> "[1 2|   3]"
    (is (= "[1 2|]"
           (-> (SUT/kill)
               (display-buffer))))))

(deftest kill-inside-whitespace-test
  "kill inside a whitespace node"
  (with-buffer
    #_>>>> "[1 2  |   3]"
    (is (= "[1 2  |]"
           (-> (SUT/kill)
               (display-buffer))))))

(deftest kill-sym-test
  "kill inside a sym"
  (with-buffer
    #_>>>> "(foo my-|symbol)"
    (is (= "(foo my-|)"
           (-> (SUT/kill)
               (display-buffer))))))

(deftest slurp-forward-test
  (with-buffer
    #_>>>> "[[1 |2] [3 4] 5]"
    (is (= "[[1 |2 [3 4]] 5]"
           (-> (SUT/slurp-forward)
               (display-buffer))))))

(deftest slurp-forward-tail-test
  "slurp forward when at the end of a list type node
   (i.e. no locator there)"
  (with-buffer
      #_>>>> "[[1 2|] [3 4] 5]"
      (is (= "[[1 2| [3 4]] 5]"
             (-> (SUT/slurp-forward)
                 (display-buffer))))))

(deftest barf-forward-test
  (with-buffer
    #_>>>> "[[1 2| [3 4]] 5]"
    (is (= "[[1 2|] [3 4] 5]"
           (-> (SUT/barf-forward)
               (display-buffer))))))

(deftest slurp-backward-test
  (with-buffer
    #_>>>> "[[1 2] [|3 4] 5]"
    (is (= "[[[1 2] |3 4] 5]"
           (-> (SUT/slurp-backward)
               (display-buffer))))))

(deftest barf-backward-test
  (with-buffer
    #_>>>> "[[[1 2]| 3 4] 5]"
    (is (= "[[1 2] |[3 4] 5]"
           (-> (SUT/barf-backward)
               (display-buffer))))))

(deftest splice-test
  (with-buffer
    #_>>>> "[[1 2|] 3]"
    (is (= "[1 2 |3]"
           (-> (SUT/splice)
               (display-buffer))))))

(deftest ^:wip splice-cursor-test
  ;; splice happens but cursor is misplaced
  (with-buffer
    #_>>>> "[1 2 [3 |4 5]]"
    (is (= "[1 2 3 |4 5]"
           (-> (rebel-readline.clojure.paredit/splice)
               display-buffer)))))


(deftest splice-at-tail-test
  (with-buffer
    #_>>>> "[[1 2|] 3]"
    (is (= "[1 2 |3]"
           (-> (SUT/splice)
               (display-buffer))))))

(deftest ^:wip split-test
  ;; split happens but cursor is misplaced
  (with-buffer
    #_>>>> "[[1| 2] 3]"
    (is (= "[[1]| [2] 3]"
           (-> (SUT/split)
               (display-buffer))))))

(deftest ^:wip split-not-ok-test
  ;; split happens but cursor is misplaced
  "this looks nearly identical to the above test
  it is still before the (node-2) but it fails"
  (with-buffer
    #_>>>> "[[1 |2] 3]"
    (is (= "[[1]| [2] 3]"
           (-> (SUT/split)
               (display-buffer))))))

(deftest ^:wip split-at-string-test
  ;; split happens but cursor is misplaced
  (with-buffer
    #_>>>> "[[1 \"some-|long-string\"] 3]"
    (is (= "[[1 \"some-\"| \"long-string\"] 3]"
           (-> (SUT/split)
               (display-buffer))))))

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
