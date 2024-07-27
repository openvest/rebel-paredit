(ns jline
  (:require [clojure.reflect :as reflect :refer [reflect]]
            [cljfmt.core :as fmt]
            [rebel-readline.jline-api :as j]
            [clojure.pprint :refer [pprint]])
  (:import [org.jline.reader.impl DefaultParser]
           [org.jline.reader.impl LineReaderImpl]
           [org.jline.terminal Terminal]
           [org.jline.terminal TerminalBuilder]
           [org.jline.terminal.impl DumbTerminal]))

(defonce terminal
  ;"after the first call, subsequent calls return a DumbTerminal"
  (-> (TerminalBuilder/builder)
      (.system true)
      (.build)))
(def line-reader (LineReaderImpl. terminal))

(def ^:dynamic *line-reader* nil)
(def ^:dynamic *buffer* nil)

(comment
;line reader builder is not panning out.  How to construct
(import '[org.jline.reader LineReaderBuilder])
  )


;; trying to find the character at a time method
;; this exits the terminal:
;; (.enterRawMode terminal)
;; look into org.jline.ConsoleReader (but rebel doesn't use this so maybe not)
(let [txt (.readLine line-reader "Enter: ")]
    (print (str "(do\n"
                "\033[32m  (thing 1)\n"
                "\033[31m  " txt "\033[0m)")))

;; not sure how to best reflect on java class methods
;; rough cut: (TODO: find a good library or reference)
(defn methods [thing]
  (->> (reflect thing)
       :members
       (filter (comp :public :flags))
       (filter (comp :parameter-types))
       #_count
       (map :name)))


(let [size (.getSize terminal)]
  [(.getRows size) (.getColumns size)])


(print (fmt/reformat-string "(defn sum [x y]\n(+ x y))"))

(let [txt (.readLine line-reader "Enter: ")
      code-str (-> (str "(do\n"
                        "(thing 1)\n"
                        txt ")")
                   ;; reformat can't handle the inline escape stuff like "\033[32m"
                   fmt/reformat-string)]
    (print code-str))
;; history
(.get (.getHistory line-reader) 1)
(.add (.getHistory line-reader) "Code did this")
;; show all history
(map #(.line %) (.getHistory line-reader))


(def p1
  (proxy [LineReaderImpl]
      [terminal
       "ov-jline"
       (java.util.HashMap. {::service (atom {})})]))
(def p2
  (proxy [LineReaderImpl]
      [terminal
       "ov-jline"
       (java.util.HashMap. {::service (atom {})})]))


(defn widget-exec [line-reader thunk]
  (binding [*line-reader* line-reader
            *buffer* (.getBuffer line-reader)]
    (try
      (thunk)
      (catch clojure.lang.ExceptionInfo e
        (if-let [message (.getMessage e)]
          (print (Throwable->map e))
          (throw e))))))

;; this is not the way to do this i.e. with self-insert
;; should be done with key-binding -> widget
(defn ov-create-line-reader [terminal app-name service]
  (let [service-variable-name (str ::service)]
    (proxy [LineReaderImpl clojure.lang.IDeref clojure.lang.IAtom]
        [terminal
         (or app-name "Rebel Readline")
         (java.util.HashMap. {service-variable-name (atom (or service {}))})]
      (selfInsert []
        ;; no widget-exec yet
        (when-let [hooks (not-empty (:self-insert-hooks @this))]
          (widget-exec this #(doseq [hook hooks] (hook))))
        (proxy-super selfInsert))
      (deref []
        (deref (.getVariable this service-variable-name)))
      (swap  [f & args]
        (apply swap! (.getVariable this service-variable-name) f args))
      (reset [a]
        (reset! (.getVariable this service-variable-name) a)))))


(defn ins-paren1
      "partially working inserter"
      [& args]
      (when (= (int \() (.prevChar *buffer*))
            (let [p (.cursor *buffer*)]
                 (.write *buffer* ")")
                 (.cursor *buffer* p)
                 (.write *buffer* "inside "))))

(def p1 (ov-create-line-reader
         terminal
         "p2"
         {:self-insert-hooks [ins-paren1 #(do (println ">:" (str *buffer*)))]}))

;; doesn't seem to work when adding a hook with swap!
;; this is no better than the above
;; when do hooks run.  can this be changed?
(defn ins-paren3
"partially working inserter"
[& args]
(locking (.writer (.getTerminal *line-reader*))
  (when (= (int \() (.prevBinding *buffer*))
    (let [p (.cursor *buffer*)]
      (.write *buffer* "<close>)")
      (.cursor *buffer* p)
      (.write *buffer* "<open>"))
    (.redisplay *line-reader*))))

(def p3 (ov-create-line-reader
         terminal
         "p3"
         {:self-insert-hooks [ins-paren3 #(do (println ">>:" (str *buffer*)))]}))

;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;
;; this takes
(let [in (io/input-stream (.getBytes "hi there"))
      out System/out]
  (-> (TerminalBuilder/builder)
      (.system false)
      (.streams in out)
      (.build)))

(let [sb (java.lang.StringBuffer.)
      in (io/input-stream (.getBytes "hi there"))
      out (proxy [java.io.OutputStream] []
            (write [news] (.append sb (char news))))
      fake-terminal (-> (TerminalBuilder/builder)
                        (.system false)
                        (.streams in out)
                        (.build))
      fake-line-reader (LineReaderImpl. fake-terminal)]
  ;; why do i need this sleep? shouldn't the .flush above here work>
  (Thread/sleep 100)
  (.readLine fake-line-reader "go> "))


;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;
;; autopairing
(import '[org.jline.widget AutopairWidgets])

(def a (AutopairWidgets. line-reader true))

(let [field (-> (.getClass a)
                (.getDeclaredField "pairs")
                (doto (.setAccessible true)))
      pairs (.get field a)]
     (doto pairs
           (.remove "`")
           (.remove "'")
           (.remove " ")))

(.enable a)

(.readLine line-reader ">>: ")

(.enable (:autopair-widgets @rebel-readline.jline-api/*line-reader*))
(.disable (:autopair-widgets @rebel-readline.jline-api/*line-reader*))
