(ns repl-balance.clojure.service.nrepl-client
  (:require
    [repl-balance.clojure.line-reader :as line-reader]
    [repl-balance.clojure.utils :as clj-utils]
    [repl-balance.clojure.main :as main]
    [repl-balance.tools :as tools]
    [repl-balance.utils :as utils]
    [repl-balance.jline-api :as j :refer [*line-reader*]]
    [clojure.repl])
  (:import
    clojure.lang.RT
    (java.io ByteArrayOutputStream
             Closeable
             EOFException
             Flushable
             PushbackInputStream
             PushbackReader)
    [java.net Socket SocketException]
    [java.util.concurrent BlockingQueue LinkedBlockingQueue SynchronousQueue TimeUnit]))

#_(defmethod line-reader/-read-string ::service [self form-str]
  (when (string? form-str)
    (try
      {:form (with-in-str form-str
                          (read {:read-cond :allow} *in*))}
      (catch Throwable e
        {:exception (Throwable->map e)}))))

(defmethod -eval-str ::service [service form-str]
  (try
    (let [msgs (nrepl/message (:client @*line-reader*) form-str)])
    (let [res (-read-string service form-str)]
      (if (contains? res :form)
        (-eval service (:form res))
        res))
    (catch Throwable e
      (set! *e e)
      {:exception (Throwable->map e)})))

;; taken from nrepl.cmdline and nrepl.core
#_(defn args->cli-options
  "Takes CLI args list and returns vector of parsed options map and
  remaining args."
  [args]
  (let [[options _args] (split-args (expand-shorthands args))
        options (->> options
                     (keywordize-options)
                     (parse-cli-values)
                     (merge config/config))]
    [options _args]))

#_(defn connection-opts
  "Takes map of nREPL CLI options
  Returns map of processed options used to connect or start a nREPL server."
  [options]
  {:port (->int (:port options))
   :host (:host options)
   :socket (:socket options)
   :transport (options->transport options)                  ; bencode
   :repl-fn (options->repl-fn options)
   :tls-keys-str (:tls-keys-str options)
   :tls-keys-file (:tls-keys-file options)})

(-> (cmdline/args->cli-options ["--port" "8282" "--host" "127.0.0.1" "--connect"])
    first
    cmdline/connection-opts
    nrepl/connect)

transport/bencode
;; nrepl.cmdline has an atom to store transport and client
;; we should have them attached to the j/*line-reader*
(let [session (nrepl/client-session client)]
  (swap! j/*line-reader* assoc :transport transport)
  (swap! j/*line-reader* assoc :client session))

(main/syntax-highlight-prn '(def x {:foo 88}))

(defn create
  ([] (create nil))
  ([options]
   (merge line-reader/default-config
          (tools/user-config)
          options
          {:repl-balance.service/type ::service})))
