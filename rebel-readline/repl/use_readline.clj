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
