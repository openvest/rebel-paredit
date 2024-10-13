(ns use-readline
  (:require repl-balance.main
            [clojure.reflect :as reflect :refer [reflect]]
            [cljfmt.core :as fmt]
            [repl-balance.jline-api :as j])
  (:import [org.jline.reader.impl DefaultParser]
           [org.jline.reader.impl LineReaderImpl]
           [org.jline.terminal Terminal TerminalBuilder]
           [org.jline.terminal.impl DumbTerminal]))

; https://www.infoworld.com/article/3697654/interactive-java-consoles-with-jline-and-consoleui.html?page=2
;; after this ctrl-d exits this line-reader
(repl-balance.main/-main)
;; Note: with no args this just calls:
;; (repl-balance.clojure.main/-main)
;; Which in turn just calls
;; (core/ensure-terminal (repl))) ; in repl-balance.clojure.main
;; and if repl-balance.jline-api/*terminal*  is already bound it is just
;; repl-balance.clojure.main/repl
;; which just calls repl-balance.clojure.main/repl*


;; proxy
;; TODO: wish I could get this to work with a clojure line-reader
;;       this does work. try to get this working for dev purposes
(let [p (proxy [org.jline.reader.impl.LineReaderImpl] [terminal, "p-proxy" {}]
               (readLine [] (str "tiny " (.getAppName this))))]
     (.readLine p))

(let [p (proxy [org.jline.reader.impl.LineReaderImpl] [terminal, "p-proxy" {}]
               (getLastBinding [] "xyz"))]
     (.getLastBinding p))

(comment
  (let [[s cur] (split-s-cur "[3 |4 5]")
        buf (doto (BufferImpl.)
                  (.write s)
                  (.cursor cur))
        a (atom (mapv str [:a :de :zyz]) )
        p (proxy [org.jline.reader.impl.LineReaderImpl] [j/*terminal*, "p-proxy" {}]
                 (getLastBinding [] (let [f  (first @a)]
                                         (swap! a rest)
                                         f)))]
       (binding [j/*line-reader* p
                 j/*buffer* buf]
                (pe/open-round)
                (join-s-cur (str j/*buffer*) (.cursor j/*buffer*)))))

;; TODO: work on testing framework
;; this works
(binding [*in* (java.io.PushbackReader. (io/reader (.getBytes "hi")))]
    (read *in*))

#_ ;; but this does not
(binding [*in* (java.io.PushbackReader. (io/reader (.getBytes "hello there")))]
  (.readLine line-reader))
;;;;;;;;;;;;;;;;;;;;;Widget copy functions
(defmacro create-widget [& body]
  `(fn [line-reader#]
     (reify Widget
       (apply [_#]
         (widget-exec line-reader# (fn [] ~@body))))))

(defn register-widget [widget-id widget]
  (doto *line-reader*
    (-> (.getWidgets)
        (.put widget-id (if (fn? widget) (widget *line-reader*) widget)))))


(defn widget-exec
"used inside create-widget macro"
[line-reader thunk]

  (binding [*line-reader* line-reader
            *buffer* (.getBuffer line-reader)]
    (try
      (thunk)
      (catch clojure.lang.ExceptionInfo e
        (if-let [message (.getMessage e)]
          (do (log :widget-execution-error (Throwable->map e))
              (display-message
               (AttributedString.
                message (.foreground AttributedStyle/DEFAULT AttributedStyle/RED)))
              true)
          (throw e))))))

(defn call-widget [widget-name]
  (.callWidget *line-reader* widget-name))

;; expanded `create-widget` macro. returns a function that takes a line-reader
;; and wraps uses that in a clojure to return a zero arg function (e.g. returns a widget)
(fn* ([line-reader__2681__auto__]
      (clojure.core/reify org.jline.reader.Widget
        (clojure.core/apply [___2682__auto__]
          (repl-balance.jline-api/widget-exec line-reader__2681__auto__
                                                (clojure.core/fn [] (do foo)))))))

;;;;;;;;;;;;;;;;;;;;;Widget copy functions


;; first success at calling a wiget manually
(binding [j/*buffer* (.getBuffer j/*line-reader*)]
  (do (.set (j/get-accessible-field j/*line-reader* "reading") j/*line-reader* true)
      (j/call-widget "kill-buffer")
      (.write (.getBuffer j/*line-reader*) "foo")
      (str j/*buffer*)))

(binding [j/*buffer* (.getBuffer j/*line-reader*)]
  (let [buf (.getBuffer j/*line-reader*)]
    (.set (j/get-accessible-field j/*line-reader* "reading") j/*line-reader* true)
    (j/call-widget "kill-buffer")
    (doto buf
      (.write "[:foo]")
      (.cursor 4))
    (j/call-widget "paredit-kill")
    (str j/*buffer*)))


(comment
  (def lr (LineReaderImpl. j/*terminal*))

  ;; works for 0 arity methods only
  ;; works: (j/invoke-private-method "SomeLongStringToSubsequence" "subSequence" (int 4) (int 14))
  ;; works: (j/invoke-private-method "someThing" "equalsIgnoreCase" "Something")
  ;; see j/invoke-private-method and j/get-private-field
  )
