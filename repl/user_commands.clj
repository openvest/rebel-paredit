(ns user-commands
  (:require [repl-balance.commands :as commands]
            [repl-balance.jline-api :as j]))

;; simple command (no args)
(commands/add-command
  :repl/pwd
  #(prn (System/getProperty "user.dir"))
  "Show current working dir")

(commands/add-command
  :repl/toggle-autopair
  (fn []
    (if (-> @j/*line-reader*
            :autopair-widgets
            (.toggle))
      (prn "autopairing is now on")
      (prn "autopairing is now off")))
  "toggle the autopairing behavior")


;; add command doesn't allow passing of params so use defmethod command
;; should this try to use "tree" if it's on unix and it's intstalled?
(defmethod commands/command :repl/ls [[_ dir]]
  (let [dir (or dir ".")]
    (->> (file-seq (clojure.java.io/file (str dir)))
         (filter #(and (.isFile %)
                       (nil? (re-find #"/\\." (str %)))))
         (mapv str)
         clojure.pprint/pprint)))
(defmethod commands/command-doc :repl/ls [_] "list files for a directory")

;; add command doesn't allow passing of params so use defmethod command
(defmethod commands/command :repl/widgets [[_ widget-name]]
  (->> (.getWidgets j/*line-reader*)
       (map (juxt #(.getKey %) #(str (.getValue %))))
       (filter #(re-find (re-pattern (or (str widget-name) "\\w")) (first %)))
       ;;(map #(.getKey %))
       ;;(filter #(re-find (re-pattern (or (str widget-name) "\\w")) %))
       (sort)
       clojure.pprint/pprint))

(defmethod commands/command-doc :repl/widgets [_]
 "list currently registered widgets.
  Widgets are typically bound to keys so look for the widget with:
  \":repl/key-bindings <widget-name>\" ")

(comment
;; elisp function/command to paste last repl-balance history command into emacs
(defun repl-balance-history ()
    (interactive)
    (with-temp-buffer
      (insert-file-contents "../.rebel_readline_history")
      (goto-char (point-max))
      (forward-line -1)
      (setq hist (replace-regexp-in-string
                  "\\\\n" "\n"
                  (buffer-substring-no-properties
                   (+ 14 (line-beginning-position))
                   (line-end-position)))))
    (push-mark (point))
    (insert hist))

)
