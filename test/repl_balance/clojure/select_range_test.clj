(ns repl-balance.clojure.select-range-test
  (:require [repl-balance.tools :as SUT]
            [repl-balance.clojure.tokenizer :as tokenizer]
            [repl-balance.clojure.sexp :as sexp]
            [repl-balance.core]
            [repl-balance.test-helpers :refer [split-s-cur join-s-cur s-cur-test reader-test]]
            [clojure.test :refer :all]))

(deftest region-1-test
  (let [s "(def x [food \"fight\"])"
        tokens (sexp/tag-font-lock+ s 0)
        region {:beg-hl 5 :end-hl 8}
        hl-tokens (SUT/tokenize-highlight+ tokens s 0 region)]
    (is (= hl-tokens
           [["_" 0 1 :font-lock/core-form]
            ["def" 1 4 :font-lock/core-form]
            ["x" 5 6 :font-lock/variable-name-highlight]
            [" [" 6 8 :insert-highlight]
            ["\"fight\"" 13 20 :font-lock/string]
            [")" 21 22 :font-lock/core-form]]))))

(deftest top-form-test
  (testing (str "select in top form> " :foo)
    (let [s ":foo"
          region {:beg-hl 2 :end-hl 3}
          hl-tokens (try
                      (-> (sexp/tag-font-lock+ s 0)
                          (SUT/tokenize-highlight+ s 0 region))
                      :OK
                      (catch Exception e (throw e) #_(ex-message e)))]
      (is (= :OK hl-tokens)))))

(deftest over-highlighting-region-test
  (testing "don't select too much"
    (let [s "[:something-longer-than-3]"
          region {:beg-hl 3 :end-hl 6}
          tokens (sexp/tag-font-lock+ s 0)
          hl-tokens (SUT/tokenize-highlight+ tokens s 0 region)]
      ;; expected hl-tokens =>
      #_ [["_" 0 1 :font-lock/core-form]
          [":" 1 2 :font-lock/constant]
          ["s" 2 3 :font-lock/constant]
          ["ome" 3 6 :font-lock/constant-highlight]
          ["thing-longer-than-3" 6 25 :font-lock/constant]
          ["]" 25 26 :font-lock/core-form]]
      (is (= ["ome" 3 6 :font-lock/constant-highlight]
             (nth hl-tokens 3)))
      (is (= ["thing-longer-than-3" 6 25 :font-lock/constant]
             (nth hl-tokens 4))))))
