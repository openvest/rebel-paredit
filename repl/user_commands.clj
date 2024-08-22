(ns user-commands
  (:require [repl-balance.commands :as commands]))

(commands/add-command
  :repl/pwd
  #(prn (System/getProperty "user.dir"))
  "Show current working dir")

(commands/add-command
  :repl/ls
  (fn [[_ dir]]
    (let [dir (or dir ".")]
     (->> (file-seq (clojure.java.io/file dir))
          (filter #(and (.isFile %)
                        (nil? (re-find #"/\." (str %)))))
          (mapv str)
          clojure.pprint/pprint)))
  "list files")

(commands/add-command
  :repl/toggle-autopair
  (fn []
    (if (-> @repl-balance.jline-api/*line-reader*
            :autopair-widgets
            (.toggle))
      (prn "autopairing is now on")
      (prn "autopairing is now off")))
  "toggle the autopairing behavior")
