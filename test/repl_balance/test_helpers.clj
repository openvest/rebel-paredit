(ns repl-balance.test-helpers
  (:require [clojure.test :refer :all]
            [clojure.string :as str])
  (:import (org.jline.reader.impl BufferImpl)))

(defn split-s-cur
  "Takes a string with a `|` to indicate the cursor position.
  Returns the corresponding `[string cursor]` pair.
  "
  [^String s+c]
  (let [cur (or (str/index-of s+c "|")
                (throw (ex-info "(with-buffer s ...) missing a | to indicate cursor" {:s s+c})))
        s (str (subs s+c 0 cur) (subs s+c (inc cur)))]
    [s cur]))

(defn ^String join-s-cur
  "Takes a buffer or [str cursor] and returns the string with a | where the cursor is"
  ([buf] (join-s-cur (str buf) (.cursor buf)))
  ([s cur]
   (str (subs s 0 cur) "|" (subs s cur))))

;; can this be a function instead of a macro?
;; looks like the function version of this below is simpler
;; the only advantage here is the ability to add the function
;; name to the `(testing ... ` output. Maybe more advantages later
;; like improved error handling so keep using this one for now
(defmacro s-cur-test
  "To test SUT/functions that take `[string cursor]` as arguments.
  The orig and target are strings that MUST include a | to indicate the cursor position"
  [str-cur-fn orig target]

  `(let [[orig-s# orig-cur#] (split-s-cur ~orig)
         [target-s# target-cur#] (split-s-cur ~target)
         [modified-s# modified-cur#] (~str-cur-fn orig-s# orig-cur#)]
     (testing (str "testing " '~str-cur-fn "  with: "~orig)
       (is (= modified-s# target-s#))
       (is (= modified-cur# target-cur#)))))

#_(defn s-cur-test
  "To test SUT/functions that take `[string cursor]` as arguments.
  The orig and target are strings that MUST include a | to indicate the cursor position"
  [str-cur-fn orig target]
  (let [[orig-s# orig-cur#] (split-s-cur orig)
         [target-s# target-cur#] (split-s-cur target)
         [modified-s# modified-cur#] (str-cur-fn orig-s# orig-cur#)]
     (testing (str "testing " #_'str-cur-fn "  with: "orig)
       (is (= modified-s# target-s#))
       (is (= modified-cur# target-cur#)))))

(defmacro buf-test
  "macro to run the body with jline-api/*buffer* bound to a buffer with the provided string
  Ths string must include a | to indicate the cursor position"
  [buf-fn orig target]
  `(let [[orig-s# orig-cur#] (split-s-cur ~orig)
        [target-s# target-cur#] (split-s-cur ~target)
        buffer# (doto (BufferImpl.)
                  (.write orig-s#)
                  (.cursor orig-cur#))]
    (~buf-fn buffer#)
    (let [modified-s# (str buffer#)
          modified-cur# (.cursor buffer#)]
      (testing (str "testing " (name '~buf-fn) " with: "~orig)
        (is (= target-s# modified-s#))
        (is (= target-cur# modified-cur#))))))

