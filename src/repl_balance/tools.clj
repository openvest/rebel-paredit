(ns repl-balance.tools
  (:require
   [repl-balance.jline-api :as api]
   [repl-balance.utils :refer [log]]
   [clojure.java.io :as io]
   [clojure.edn :as edn])
  (:import
   [org.jline.utils AttributedStringBuilder AttributedStyle]
   [org.jline.keymap KeyMap]))

;; ----------------------------------------------
;; Extra Abilities
;; ----------------------------------------------

;; Color
;; ----------------------------------------------

(def color-themes {})

(defn register-color-theme! [ky color-map]
  (assert (keyword? ky))
  (assert (map? color-map))
  (alter-var-root #'color-themes assoc ky color-map))

(defn fg-color [color]
  (.foreground AttributedStyle/DEFAULT color))

(defn color [sk]
  (->
   (get @api/*line-reader* :color-theme)
   color-themes
   (get sk AttributedStyle/DEFAULT)))

;; String Highlighting
;; ----------------------------------------------

(defn ^AttributedStringBuilder highlight-tokens [color-fn tokens syntax-str]
  (let [sb (AttributedStringBuilder.)
        syntax-str-len (count syntax-str)]
    (loop [pos 0
           hd tokens]
      (let [[_ start end sk] (first hd)]
        (cond
          ;; we are done
          (= (.length sb) syntax-str-len)
          sb
          ;; we are almost done, no more formatting so just append
          (nil? start)
          (doto sb (.append (subs syntax-str pos)))
          ;; do styled substring
          (= start pos)                                     ;; style active
          (do
            (if-let [st (color-fn sk)]
              (.styled sb st (subs syntax-str start end))
              (.append sb (subs syntax-str start end)))
            (recur end (rest hd)))
          ;; do unformatted substring
          (> start pos)
          (do (.append sb (subs syntax-str pos start))
              (recur start hd))
          ;; this should not happen
          ;; send a display-message or throw an error?
          :else
          (do
            (.append sb (.charAt syntax-str pos))
            (recur (inc pos) hd)))))))

(defn highlight-str [color-fn tokenizer-fn syntax-str]
  (highlight-tokens color-fn (tokenizer-fn syntax-str) syntax-str))

;; Baseline service config
;; ----------------------------------------------

;; follow tools deps conventions

(defn user-config-file []
  (->> [(System/getenv "CLJ_CONFIG")
        (some-> (System/getenv "XDG_CONFIG_HOME")
                (io/file "clojure"))
        (io/file (System/getProperty "user.home")
                 ".clojure")]
       (keep identity)
       (map #(io/file % "repl_balance.edn"))
       (filter #(.exists %))
       first))

(defn translate-serialized-key-bindings [key-binding-map]
  (into {}
        (map
         (fn [[map-name binds]]
           [map-name
            (mapv (fn [[k v]]
                    [(cond
                       (string? k)
                       (KeyMap/translate k)
                       ;; bypass translation with a list of ints and chars
                       (and (list? k) (every? #(or (integer? %) (char? %)) k))
                       (apply str (map char k))
                       (vector? k)
                       (mapv #(KeyMap/translate %) k)) v])
                  binds)])
         key-binding-map)))

(defn user-config []
  (when-let [file (user-config-file)]
    (try (let [config (edn/read-string (slurp file))]
           (cond-> config
             (:key-bindings config)
             (update-in [:key-bindings] translate-serialized-key-bindings)))
         (catch Throwable e
           (binding [*out* *err*]
             (println (format "[Repl Balance] Error reading config file %s: %s"
                              (str file)
                              (.getMessage e))))))))

;; Baseline services
;; ----------------------------------------------

(defn resolve-fn? [f]
  (cond
    (fn? f) f
    (or (string? f) (symbol? f))
    (resolve (symbol f))
    :else nil))

(defn not-implemented! [service fn-name]
  (throw (ex-info (format "The %s service does not implement the %s function."
                          (pr-str (::type service))
                          fn-name)
                  {})))

(defn service-dispatch [a & args] (:repl-balance.service/type a))

;; Prompt
;; ----------------------------------------------

(defmulti -prompt
  "returns a read-line prompt string"
  service-dispatch)

(defmethod -prompt :default [_] "")

(defn prompt []
  (log :prompt-fn @api/*line-reader*)
  (if-let [f (resolve-fn? (:prompt @api/*line-reader*))]
    ;; follow the prompt function convention here
    (with-out-str (f))
    (-prompt @api/*line-reader*)))

;; Initial Themes

(def dark-screen-theme
  {:font-lock/string         (.bold (fg-color 180))
   :font-lock/comment        (.bold (fg-color 243))
   :font-lock/doc            (.bold (fg-color 223))
   :font-lock/core-form      (.bold (fg-color 39))
   :font-lock/function-name  (.bold (fg-color 178))
   :font-lock/variable-name  (.bold (fg-color 85))
   :font-lock/constant       (.bold (fg-color 149))
   :font-lock/type           (.bold (fg-color 123))
   :font-lock/foreign        (.bold (fg-color 220))
   :font-lock/builtin        (.bold (fg-color 167))
   :region-background        26

   :widget/half-contrast     (fg-color 243)
   :widget/half-contrast-inverse (.inverse (fg-color 243))

   ;; system widget colors
   :widget/eldoc-namespace   (.faint (fg-color 123))
   :widget/eldoc-varname     (.faint (fg-color 178))
   :widget/eldoc-separator   (fg-color 243)
   :widget/arglists          (fg-color 243)

   :widget/doc               (fg-color 222)
   :widget/anchor            (fg-color 39)
   :widget/light-anchor      (.faint (fg-color 39))

   :widget/apropos-word      AttributedStyle/DEFAULT
   :widget/apropos-highlight (fg-color 45)
   :widget/apropos-namespace (.faint (fg-color 243))

   :widget/warning           AttributedStyle/DEFAULT
   :widget/error             (fg-color 196)})



(register-color-theme! :dark-screen-theme dark-screen-theme)

;; TODO fix these
(def light-screen-theme
  (assoc dark-screen-theme
         :font-lock/type            (.bold (fg-color 28))
         :font-lock/constant        (.bold (fg-color 31))
         :font-lock/function-name   (.bold  (fg-color 21))
         ;:font-lock/core-form      (.bold  (fg-color 21))
         :font-lock/variable-name   (.bold (fg-color 130))
         :font-lock/core-form       (.bold (fg-color 127))
         :font-lock/string          (.bold (fg-color 127))
         :font-lock/foreign         (.bold (fg-color 97))
         :font-lock/doc             (.bold (fg-color 132))
         :font-lock/comment         (.bold (fg-color 247))
         :region-background         153

         :widget/eldoc-namespace    (fg-color 28)
         :widget/eldoc-varname      (fg-color 21)
         ;:widget/eldoc-separator   (fg-color 243)
         ;:widget/arglists          (fg-color 243)

         :widget/doc                (fg-color 238)
         :widget/light-anchor       (.underline (.faint (fg-color 26)))

         :widget/apropos-word       AttributedStyle/DEFAULT
         :widget/apropos-highlight  (fg-color 27)
         :widget/apropos-namespace  (fg-color 243)))

(register-color-theme! :light-screen-theme light-screen-theme)

(def neutral-screen-theme
  {:font-lock/string         (.bold (fg-color 2))
   :font-lock/comment        (.bold (fg-color 8))
   :font-lock/doc            (.bold (fg-color 5))
   :font-lock/core-form      (.bold (fg-color 2))
   :font-lock/function-name  (.bold (fg-color 6))
   :font-lock/variable-name  (.bold (fg-color 3))
   :font-lock/constant       (.bold (fg-color 4))
   :font-lock/type           (.bold (fg-color 1))
   :font-lock/foreign        (.bold (fg-color 1))
   :font-lock/builtin        (.bold (fg-color 6))
   :region-background        153

   :widget/half-contrast     (fg-color 7)
   :widget/half-contrast-inverse (.inverse (fg-color 7))

   ;; system widget colors
   :widget/eldoc-namespace   (.faint (fg-color 2))
   :widget/eldoc-varname     (.faint (fg-color 3))
   :widget/eldoc-separator   (fg-color 4)
   :widget/arglists          (fg-color 5)

   :widget/doc               (fg-color 1)
   :widget/anchor            (fg-color 5)
   :widget/light-anchor      (.faint (fg-color 5))

   :widget/apropos-word      AttributedStyle/DEFAULT
   :widget/apropos-highlight (fg-color 7)
   :widget/apropos-namespace (.faint (fg-color 7))

   :widget/warning           AttributedStyle/DEFAULT
   :widget/error             (fg-color 1)})

(register-color-theme! :neutral-screen-theme neutral-screen-theme)

;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;

(defn color+
  "replacement for tools/color that does the same lookup
   but if not found attempts to find the style without -highlight
   and adds background color"
  [sk]
  (let [color-map (-> (get @api/*line-reader* :color-theme)
                      color-themes)]
    (if-let [style (get color-map sk)]
      style
      ;; TODO: this is suspect, should we just add these to the color-map?
      (if-let [[_ sub-tag] (re-matches #"(.*)-highlight" (name sk))]
        (-> (keyword (namespace sk) sub-tag)
            (color-map AttributedStyle/DEFAULT)
            (.background (get color-map :region-background 20)))
        AttributedStyle/DEFAULT))))

(defn highlight-keyword+
  "take a keyword and append -highlight to it"
  [kw]
  (keyword (namespace kw) (str (name kw) "-highlight")))

;; TODO: loop recur? or lazy-seq?
(defn tokenize-highlight+
  "takes tokens and a region with [beg-hl end-hl] keys and
  return tokens with the highlighting tags added to the region"
  [tokens s cur {:keys [beg-hl end-hl] :as region}]
  (let [[sub beg end tag :as token] (first tokens)]
    (if (nil? token)
      (when (<= cur end-hl)
        ;; no more tokens but more to highlight
        [[(subs s cur end-hl) cur end-hl :insert-highlight]])
      (cond
        ;; token ends before highlighting begins (b)
        (<= end beg-hl)
        (cons token
              (tokenize-highlight+ (rest tokens) s end region))
        ;; highlighting started on or after cursor but before this token
        (and (<= beg-hl beg) (< cur beg) (< cur end-hl))
        (let [b (max cur beg-hl)
              e (min beg end-hl)]
          (if (< b e)
            (cons [(subs s b e) b e  :insert-highlight]
                  (tokenize-highlight+ tokens s e region))
            (tokenize-highlight+ tokens s e region)))
        ;; token entirely within the highlight region (d)
        (and (= beg cur) (<= beg-hl beg) (<= end end-hl))
        (cons [sub beg end (highlight-keyword+ tag)]
              (tokenize-highlight+ (rest tokens) s end region))
        ;; highlighting ended before token but after prev cursor
        (<= end-hl beg)
        (if (< cur end-hl)
          ;; produce new token to highlight plain text (e)
          (let [b (max cur beg-hl)
                e (min beg end-hl)]
            (concat [[(subs s b e) b e  :insert-highlight]]
                    tokens))
          tokens)
        ;; token begins after highlighting began but ends before highlighting ends
        ;; first half highlighted second half not highlighted
        (and (<= beg-hl beg) (< end-hl end))
        (let [b (max cur beg-hl)
              e (min beg end-hl)]
          (concat [[(subs s b end-hl) b end-hl (highlight-keyword+ tag)]
                   [(subs s end-hl end) end-hl end tag]]
                  (tokenize-highlight+
                    (rest tokens) s end region)))
        ;; token began before highlighting began
        ;; first half not highlighted second half highlighted
        (< beg beg-hl)
        (concat [[(subs s beg beg-hl)
                  beg beg-hl tag]
                 [(subs s beg-hl (min end-hl end))
                  beg-hl (min end-hl end) (highlight-keyword+ tag)]]
                (when (< end-hl end)
                  [[(subs s end-hl end)
                    end-hl end tag]])
                (tokenize-highlight+
                  (rest tokens) s end region))
        ;; should have covered everything
        :default
        [[(str cur " " tag) beg end :error]]
        #_(cons (tokenize-highlight+ (rest tokens) s end region))))))
