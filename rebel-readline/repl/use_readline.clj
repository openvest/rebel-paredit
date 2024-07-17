(ns use-readline
  (:require rebel-readline.main
            [rebel-readline.jline-api :as j]))
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
(let [p (proxy [org.jline.reader.impl.LineReaderImpl] [terminal, "p-proxy" {}]
               (readLine [] (str "tiny " (.getAppName this))))]
     (.readLine p))
