(ns colorize
  (:require repl-balance.main
            [repl-balance.tools :as t]
            [repl-balance.jline-api :as j]
            [repl-balance.clojure.line-reader :as line-reader]
            [repl-balance.clojure.tokenizer :as tokenizer]
            [repl-balance.clojure.sexp :as sexp]
            [clojure.pprint :as pprint :refer [pprint]])
  (:import [org.jline.utils AttributedStringBuilder AttributedStyle]))

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
(j/->ansi *1)

(repl-balance.clojure.main/syntax-highlight-prn '(def x {:foo 88}))
; calls this line:
(println (api/->ansi (line-reader/highlight-clj-str (pr-str x))))
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
(-> (line-reader/clojure-highlighter)
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

; direct version
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

;; highliting selected text

(defn color
  "replacement for tools/color that does the same lookup
   but if not found attempts to find the style without -highlight
   and adds background color"
  [sk]
  (let [color-map (-> (get @j/*line-reader* :color-theme)
                      t/color-themes)]
    (if-let [style (get color-map sk)]
      style
      (if-let [[_ sub-tag] (re-matches #"(.*)-highlight" (name sk))]
        (-> (keyword (namespace sk) sub-tag)
            (get color-map AttributedStyle/DEFAULT)
            (.background 242))
        AttributedStyle/DEFAULT))))

(defn highlight-keyword
  "take a keyword and append -highlight to it"
  [kw & suffexes]
  (keyword (namespace kw) (apply str (name kw) "-highlight" suffexes)))

(defn tokenize-highlight
  "takes tokens and a region with [beg-hl end-hl] keys and
  return tokens with the highliting tags added to the region"
  [tokens s cur {:keys [beg-hl end-hl] :as region}]
  (let [[sub beg end tag :as token] (first tokens)]
    (println token)
    (print (str cur " " (subs s 0 cur) "|" (subs s cur) "\n\n"))
    (if (and (nil? token) (<= cur end-hl))
      ;; no more tokens but more to highlight
      [[(subs s cur end-hl) cur end-hl :insert-highlight-a]]
      (cond
        ;; token ends before highlighting begins (b)
        (<= end beg-hl)
        (cons token
              (tokenize-highlight (rest tokens) s end region))
        ;; highlighting started on or after cursor but before this token
        (and (< beg-hl beg) #_(< cur beg) (< cur end-hl))
        (let [b (max cur beg-hl)
              e (min beg end-hl)]
          (println (subs s b e) b e  :insert-highlight-c)
          (if (< b e)
           (cons [(subs s b e) b e  :insert-highlight-c]
                 (tokenize-highlight tokens s e region))
           (tokenize-highlight tokens s e region)))
        ;; token entirely within the highlight region (d)
        (and (= beg beg-hl) (<= end end-hl))
        (cons [sub beg end (highlight-keyword tag "-d")]
              (tokenize-highlight (rest tokens) s end region))
        ;; highliting ended before token but after prev cursor
        (<= end-hl beg)
        (if (< cur end-hl)
          ;; produce new token to highlight plain text (e)
          (let [b (max cur beg-hl)
                e (min beg end-hl)]
            (concat [[(subs s b e) b e  :insert-highlight-e]]
                  tokens))
          tokens)
        ;; token begins after highlighting began but ends before highliting ends
        (< beg-hl cur beg)
        (let [b (max cur beg-hl)
              e (min beg end-hl)]
            (cons [(subs s b e) b e  (highlight-keyword tag "-f")]
                  tokens))
        ;; token began before highlighting began
        ;; first half is still unhilighted second half highlited
        (< beg beg-hl)
        (concat [[(subs s beg beg-hl)
                  beg beg-hl tag]
                 [(subs s beg-hl end)
                  beg-hl end (highlight-keyword tag "-g")]]
                (tokenize-highlight
                 (rest tokens) s end region))
        ;; should have covered everything
        :default
        (cond ["_" 0 0 :error]
              (tokenize-highlight (rest tokens) s end region))))))

(comment
  ,
  (def-let [s "(def x [food \\"fight\\"])"
            tokens (sexp/tag-font-lock+ s 0)
            region {:beg-hl 3 :end-hl 20}
            hl-tokens (tokenize-highlight tokens s 0 region)]
    (println (subs s (:beg-hl region) (:end-hl region))) 
    (-> (t/highlight-tokens color hl-tokens s)
        (j/->ansi)
        print))
  ,
  )
