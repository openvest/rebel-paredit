(ns colorize
  (:require rebel-readline.main
            [rebel-readline.jline-api :as j]))


;; color fn e.g. (rebel-readline.tools/color  :font-lock/variable-name)
rebel-readline.tools/colorize
;; tokenized string:
(rebel-readline.clojure.tokenizer/tag-font-lock "(def x 8)")
;; => (["def" 1 4 :font-lock/core-form] ["x" 5 6 :font-lock/variable-name])

;; returns AttributedStringBuilder
(rebel-readline.tools/highlight-tokens
  rebel-readline.tools/color
  [["def" 1 4 :font-lock/core-form]
   ["x" 5 6 :font-lock/variable-name]]
  "(def x 8)")
;; final step
(rebel-readline.jline-api.attributed-string/->ansi-256 *1)

(rebel-readline.clojure.main/syntax-highlight-prn '(def x {:foo 88}))
; calls this line:
(println (api/->ansi (clj-line-reader/highlight-clj-str (pr-str x))))
; which uses a tag-keyword->color fn and a str->tokenized str function
; returns AttributedStringBuilder
(rebel-readline.tools/highlight-tokens
  rebel-readline.tools/color
  [["def" 1 4 :font-lock/core-form]
   ["x" 5 6 :font-lock/variable-name]]
  "(def x 8)")
;passed to jline_api.astring/->ansi-256

(defn colorize
      [s]
      (-> (rebel-readline.clojure.tokenizer/tag-font-lock s)
          (#(rebel-readline.tools/highlight-tokens rebel-readline.tools/color % s))
          ;(rebel-readline.jline-api.attributed-string/->ansi-256)
          (j/->ansi)))
