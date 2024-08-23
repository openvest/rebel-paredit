(ns colorize
  (:require repl-balance.main
            [repl-balance.jline-api :as j]))

;; colorizing specific
;; puget
;;  https://github.com/greglook/puget
;; note that repl-balance has its own tokenizer and colorizer

;; color fn e.g. (repl-balance.tools/color  :font-lock/variable-name)
repl-balance.tools/colorize
;; tokenized string:
(repl-balance.clojure.tokenizer/tag-font-lock "(def x 8)")
;; => (["def" 1 4 :font-lock/core-form] ["x" 5 6 :font-lock/variable-name])

;; returns AttributedStringBuilder
(repl-balance.tools/highlight-tokens
  repl-balance.tools/color
  [["def" 1 4 :font-lock/core-form]
   ["x" 5 6 :font-lock/variable-name]]
  "(def x 8)")
;; final step
(repl-balance.jline-api.attributed-string/->ansi-256 *1)

(repl-balance.clojure.main/syntax-highlight-prn '(def x {:foo 88}))
; calls this line:
(println (api/->ansi (clj-line-reader/highlight-clj-str (pr-str x))))
; which uses a tag-keyword->color fn and a str->tokenized str function
; returns AttributedStringBuilder
(repl-balance.tools/highlight-tokens
  repl-balance.tools/color
  [["def" 1 4 :font-lock/core-form]
   ["x" 5 6 :font-lock/variable-name]]
  "(def x 8)")
;passed to jline_api.astring/->ansi-256

(defn colorize
      [s]
      (-> (repl-balance.clojure.tokenizer/tag-font-lock s)
          (#(repl-balance.tools/highlight-tokens repl-balance.tools/color % s))
          ;(repl-balance.jline-api.attributed-string/->ansi-256)
          (j/->ansi)))

;; print something in red
(-> (repl-balance.tools/highlight-tokens
      repl-balance.tools/color
      [["_" 12 15 :widget/error]]
      "Autopair is OFF")
    (j/->ansi )
    print )

