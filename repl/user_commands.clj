(ns user-commands
  (:require [repl-balance.commands :as commands]))

;; simple command (no args)
(commands/add-command
  :repl/pwd
  #(prn (System/getProperty "user.dir"))
  "Show current working dir")

;; add command doesn't allow passing of params so use defmethod command
(defmethod commands/command :repl/ls [[_ dir]]
  (let [dir (or dir ".")]
    (->> (file-seq (clojure.java.io/file (strdir)))
         (filter #(and (.isFile %)
                       (nil? (re-find #"/\\." (str %)))))
         (mapv str)
         clojure.pprint/pprint)))
(defmethod commands/command-doc :repl/ls [_] "list files for a directory")


(commands/add-command
  :repl/toggle-autopair
  (fn []
    (if (-> @repl-balance.jline-api/*line-reader*
            :autopair-widgets
            (.toggle))
      (prn "autopairing is now on")
      (prn "autopairing is now off")))
  "toggle the autopairing behavior")

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
