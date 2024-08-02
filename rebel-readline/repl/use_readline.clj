(ns use-readline
  (:require rebel-readline.main
            [clojure.reflect :as reflect :refer [reflect]]
            [cljfmt.core :as fmt]
            [rebel-readline.jline-api :as j])
  (:import [org.jline.reader.impl DefaultParser]
           [org.jline.reader.impl LineReaderImpl]
           [org.jline.terminal Terminal]
           [org.jline.terminal TerminalBuilder]
           [org.jline.terminal.impl DumbTerminal]))

; https://www.infoworld.com/article/3697654/interactive-java-consoles-with-jline-and-consoleui.html?page=2
;; after this ctrl-d exits this line-reader
(rebel-readline.main/-main)
;; Note: with no args this just calls:
;; (rebel-readline.clojure.main/-main)
;; Which in turn just calls
;; (core/ensure-terminal (repl))) ; in rebel-readline.clojure.main
;; and if rebel-readline.jline-api/*terminal*  is already bound it is just
;; rebel-readline.clojure.main/repl
;; which just calls rebel-readline.clojure.main/repl*


;; proxy
;; TODO: wish I could get this to work with a clojure line-reader
;;       this does work. try to get this working for dev purposes
(let [p (proxy [org.jline.reader.impl.LineReaderImpl] [terminal, "p-proxy" {}]
               (readLine [] (str "tiny " (.getAppName this))))]
     (.readLine p))

;; TODO: work on testing framework
;; this works
(binding [*in* (java.io.PushbackReader. (io/reader (.getBytes "hi")))]
    (read *in*))

#_ ;; but this does not
(binding [*in* (java.io.PushbackReader. (io/reader (.getBytes "hello there")))]
  (.readLine line-reader))


(def ap (:autopair-widgets @line-reader))
(def ap-can-skip-field
  (doto
    (.getDeclaredMethod (class ap) "canSkip" (into-array [java.lang.String]))
    (.setAccessible true)))

(comment
  ;; TODO invoke not working
  (.invoke ap-can-skip-field ap (into-array Object ["hi"]))
  (def a (into-array ["hi"]))

  (def lr (LineReaderImpl. j/*terminal*))
  (def ap (proxy [AutopairWidgets] [lr true]
            )))

;; can't get this to work
;; works for 0 arity methods only
;; works: (invoke-private-method "SomeLongStringToSubsequence" "subSequence" (int 4) (int 14))
;; works: (invoke-private-method "someThing" "equalsIgnoreCase" "Something")
(defn invoke-private-method
  [obj fn-name & args]
  (let [m (->> (.. obj getClass getDeclaredMethods)
               (filter (fn [x] (.. x getName (equals fn-name))))
               (filter (fn [x] (= (count args) (.getParameterCount x))))
               first)
        m-args (into-array Object args)]
    (.setAccessible m true)
    (.invoke m obj m-args)))

;; seems to work OK
(defn get-private-field
  [obj fn-name]
  (let [f (->> (.. obj getClass getDeclaredFields)
               (filter (fn [x] (.. x getName (equals fn-name))))
               first)]
    (.setAccessible f true)
    (.get f obj)))
