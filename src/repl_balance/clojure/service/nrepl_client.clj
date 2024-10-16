(ns repl-balance.clojure.service.nrepl-client
  (:require
    [repl-balance.clojure.line-reader :as clj-reader]
    [repl-balance.clojure.utils :as clj-utils]
    [repl-balance.tools :as tools]
    [repl-balance.utils :as utils]
    [clojure.repl]))

#_(defmethod clj-reader/-read-string ::service [self form-str]
  (when (string? form-str)
    (try
      {:form (with-in-str form-str
                          (read {:read-cond :allow} *in*))}
      (catch Throwable e
        {:exception (Throwable->map e)}))))



(defn create
  ([] (create nil))
  ([options]
   (merge clj-reader/default-config
          (tools/user-config)
          options
          {:repl-balance.service/type ::service})))
