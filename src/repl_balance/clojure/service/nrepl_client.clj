(ns repl-balance.clojure.service.nrepl-client
  (:require
    [repl-balance.clojure.line-reader :as line-reader]
    [repl-balance.clojure.utils :as clj-utils]
    [repl-balance.clojure.main :as main]
    [repl-balance.tools :as tools]
    [repl-balance.utils :as utils]
    [repl-balance.jline-api :as j :refer [*line-reader*]]
    [clojure.repl]
    ;; nrepl modules
    [nrepl.bencode :as bencode]
    [nrepl.core :as nrepl]
    [nrepl.cmdline :as cmdline]
    [nrepl.transport :as transport]
    [nrepl.util.threading])
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

;;;;;;;;;;;;;;;;;;; copied from nrepl ;;;;;;;;;;;;;;;;;;;

(defn prn-client
  "print syntax highlighted string from another thread"
  [resp]
  (binding [*out* (.. *line-reader* getTerminal writer)]
    (->> resp
         line-reader/highlight-clj-str
         j/->ansi
         println))
  (flush))

;; should be using something like
;; (threading/run-with @threading/transport-executor ...
;; is that really better than a future?
(defn nrepl-printer
  "create a  (cancellable) future to read and print the client
   from another long-running thread"
  [transport]
  (some-> (:print-future @*line-reader*) (future-cancel))
  (let [fut (future (->> (repeatedly #(transport/recv transport))
                         (take-while identity)
                         ;; (map #(cond-> % :changed-namespaces (dissoc :changed-namespaces)))
                         (map #(condp apply [%]
                                 :value :>> prn-client
                                 :out :>> println
                                 :ex (println (juxt :ex :root-ex) %)
                                 nil))
                         doall
                         count))]
    (swap! *line-reader* assoc :print-future fut)))

(derive ::service ::line-reader/clojure)
#_(defmethod line-reader/-read-string ::service [self form-str]
  (when (string? form-str)
    (try
      {:form (with-in-str form-str
                          (read {:read-cond :allow} *in*))}
      (catch Throwable e
        {:exception (Throwable->map e)}))))

(defmethod line-reader/-eval-str ::service [service form-str]
  (try
    (let [msgs (nrepl/message (:client @*line-reader*) form-str)])
    (let [res (line-reader/-read-string service form-str)]
      (if (contains? res :form)
        (line-reader/-eval service (:form res))
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


(do
  ;smallest socket opener just a java socket
  (def socket (java.net.Socket. "127.0.0.1" 8282))
  ;smallest transport
  ;socket wrapped with bencode that can send and recv a map
  ;returns an object that implements the send, recv and close methods
  (def transport (transport/bencode socket))
  (swap! j/*line-reader* assoc :transport transport)
  (nrepl-printer transport)
  )

;; nrepl.cmdline has an atom to store transport and client
;; we should have them attached to the j/*line-reader*
(comment
  ;; transport does some threading stuff to send or recv a map
  (transport/send transport {:op :eval :code "(inc 99)"})
  (transport/recv transport 5000)

  ;; client collects up recv messages
  (def client (nrepl/client transport 3600000)) ;; will return nil after 1 hr
  ;; session is the same as a client but with a sticky session-id
  (def session (nrepl/client-session client))
  ;;(nrepl/message session {:op :eval :code "(gensym 'some_response_)"})
  (swap! j/*line-reader* assoc :client session)
  )

(comment
  (def nrepl-client
    (j/create-widget
      (do (let [transport (:transport @j/*line-reader*)
                code-str (str (.getBuffer j/*line-reader*))]
            (transport/send transport {:op :eval :code code-str}))
          true)))

  (j/register-widget "nrepl-client" nrepl-client)

  (j/key-binding :emacs (str (KeyMap/ctrl \X) (KeyMap/ctrl \X)) "nrepl-client")
  (j/apply-key-bindings!)
  (j/set-main-key-map! :emacs)
  )

(defn create
  ([] (create nil))
  ([options]
   (merge line-reader/default-config
          (tools/user-config)
          options
          {:repl-balance.service/type ::service})))

