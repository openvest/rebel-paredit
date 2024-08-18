(ns repl-balance.test-helpers
  (:require [clojure.test :refer :all]
            [clojure.string :as str]))

(defn split-s-cur
  [^String s+c]
  (let [cur (or (str/index-of s+c "|")
                (throw (ex-info "(with-buffer s ...) missing a | to indicate cursor" {:s s+c})))
        s (str (subs s+c 0 cur) (subs s+c (inc cur)))]
    [s cur]))

(defn ^String join-s-cur
  "takes a buffer or [str cursor] and returns the string with a | where the cursor is"
  ([buf] (join-s-cur (str buf) (.cursor buf)))
  ([s cur]
   (str (subs s 0 cur) "|" (subs s cur))))

(defmacro s-cur-test
  "macro to run the body with jline-api/*buffer* bound to a buffer with the provided string
  Ths string must include a | to indicate the cursor position"
  [str-cur-fn orig target]

  `(let [[orig-s# orig-cur#] (split-s-cur ~orig)
         [target-s# target-cur#] (split-s-cur ~target)
         [modified-s# modified-cur#] (~str-cur-fn orig-s# orig-cur#)]
     (testing (str "testing " ~str-cur-fn "  with: "~orig)
       (is (= target-s# modified-s#))
       (is (= target-cur# modified-cur#)))))
