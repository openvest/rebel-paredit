(ns colorize
  (:require repl-balance.main
            [repl-balance.tools :as t]
            [repl-balance.jline-api :as j]
            [repl-balance.clojure.line-reader :as line-reader]
            [repl-balance.clojure.tokenizer :as tokenizer]
            [repl-balance.clojure.sexp :as sexp]
            [repl-balance.test-helpers :refer [split-s-cur join-s-cur]]
            [clojure.pprint :as pprint :refer [pprint]])
  (:import [org.jline.utils AttributedStringBuilder AttributedString AttributedStyle]))

;; colorizing specific
;; puget
;;  https://github.com/greglook/puget
;; note that repl-balance has its own tokenizer and colorizer

;; color fn e.g. (repl-balance.tools/color  :font-lock/variable-name)

;; tokenized string:
(tokenizer/tag-font-lock "(def x 8)")
;; => (["def" 1 4 :font-lock/core-form] ["x" 5 6 :font-lock/variable-name])

;; returns AttributedStringBuilder
(t/highlight-tokens
  t/color
  [["def" 1 4 :font-lock/core-form]
   ["x" 5 6 :font-lock/variable-name]]
  "(def x 8)")
;; final step
;; (j/->ansi *1)

(repl-balance.clojure.main/syntax-highlight-prn '(def x {:foo 88}))
; calls this line:
;(println (j/->ansi (line-reader/highlight-clj-str (pr-str x))))
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
      (-> (tokenizer/tag-font-lock s)
          (#(t/highlight-tokens t/color % s))
          (j/->ansi)))

;; print something in red
(-> (t/highlight-tokens
      t/color
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

(def service (repl-balance.clojure.service.local/create))

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
(-> (line-reader/clojure-highlighter+)
    (.highlight (line-reader/create service) "(def foo [:bar :food])")
    (j/->ansi)
    (print))

;; show all colors to pick one for selection
(for [i (range 256)]
  (j/->ansi (AttributedString.
             (format  "bg: %3s" i)
             (.background (t/fg-color 178) i))))

;; how faint is faint?
(for [i (range 256)]
  (j/->ansi (AttributedString.
             (format  "faint:%3s" i)
             (.faint (t/fg-color i)))))

; direct versionna
(-> (AttributedStringBuilder.)
    (.append "hi ")
    (.styled (t/fg-color 120) "there")
    (.toAttributedString)
    (.toAnsi j/*terminal*))
; indirect version  using j/->ansi
(-> (AttributedStringBuilder.)
    (.append "hi ")
    (.styled (t/fg-color 120) "there")
    j/->ansi)

;; highlighting selected text

(comment
  ,
  (def-let [s "(def x [food \"fight])"
            tokens (sexp/tag-font-lock+ s 0)
            region {:beg-hl 3 :end-hl 20}
            hl-tokens (tokenize-highlight tokens s 0 region)]
    (println (subs s (:beg-hl region) (:end-hl region)))
    (-> (t/highlight-tokens color hl-tokens s)
        (j/->ansi)
        print))
  ,

  (let [s "(def xyz [food :fight])"
        tokens (sexp/tag-font-lock+ s 0)]
    (for [beg-hl (range (- (count s) 2))
          :let [region {:beg-hl beg-hl :end-hl (+ beg-hl 3)}
                hl-tokens (tokenize-highlight+ tokens s 0 region) ]]
      (do (-> (t/highlight-tokens color+ hl-tokens s)
              (j/->ansi)
              print)
          (println region))))
  )
