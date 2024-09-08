(ns colorize
  (:require repl-balance.main
            [repl-balance.tools :as t]
            [repl-balance.jline-api :as j]
            [repl-balance.clojure.tokenizer :as tokenizer]))

;; colorizing specific
;; puget
;;  https://github.com/greglook/puget
;; note that repl-balance has its own tokenizer and colorizer

;; color fn e.g. (repl-balance.tools/color  :font-lock/variable-name)

;; tokenized string:
(repl-balance.clojure.tokenizer/tag-font-lock "(def x 8)")
;; => (["def" 1 4 :font-lock/core-form] ["x" 5 6 :font-lock/variable-name])

;; returns AttributedStringBuilder
(t/highlight-tokens
  t/color
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
    j/->ansi
    print )

(let [s "(def x [:foo \"hi there\"])"
          cur 8
          ts (tokenizer/tag-sexp-traversal s)
          tf (tokenizer/tag-font-lock s)
          br (map #(-> % pop (conj :widget/error))
                 [(sexp/find-open-sexp-start ts cur)
                  (sexp/find-open-sexp-end ts cur)] )
          tokens (sort #(- (second %1) (second %2)) (concat  tf br))]
  (print (j/->ansi (t/highlight-tokens t/color tokens s))))

(-> "(def x :abc)"
    line-reader/highlight-clj-str
    j/->ansi)


;; using the new highlighter
(binding [j/*line-reader* (line-reader/create service)
          j/*buffer* (apply j/buffer* (split-s-cur "(def| foo [:bar :food])")) ]
  (-> (line-reader/clojure-highlighter+)
    (.highlight j/*line-reader* (str j/*buffer*))
    (j/->ansi)
    (print)))


;; see all colors
(doseq [k (keys (:dark-screen-theme t/color-themes))]
  (-> (t/highlight-tokens t/color
         [["_" 0 (count (str k)) k]] (str k \newline))
      (j/->ansi)
      (print)))

;;
(-> (line-reader/clojure-highlighter)
    (.highlight (line-reader/create service) "(def foo [:bar :food])")
    (j/->ansi)
    (print))
