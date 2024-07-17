(ns key-binding
  (:require [rebel-readline.jline-api :as j]
            [clojure.pprint :as pprint :refer [pprint]]))


;; bind a key to a widget name
;; why doesn't this work
;(swap! j/*line-reader* update-in  [:key-bindings :main]   (fnil conj []) ["U" "clojure-force-accept-line"])
(swap! j/*line-reader* update-in  [:key-bindings :main]   #((fnil conj []) %1 (first %2)) ["U" "clojure-force-accept-line"])
(j/apply-key-bindings!)
;; something funky with swap! calling the proxy swap which does an (apply swap! ...

;; but this does work
(swap! j/*line-reader* update :key-bindings (constantly {:main [["C" "clojure-force-accept-line"]]}))
(j/apply-key-bindings!)

;; wierd and wonky but this rows too
(swap! j/*line-reader* update-in  [:key-bindings :emacs]
       #((fnil conj []) %1 (first %2))
       [(apply str (map char [24 24])) "beginning-of-buffer"])
(j/apply-key-bindings!)

;;;;
(comment
  ;;remove :key-bindings
  (swap! j/*line-reader* dissoc :key-bindings)
  ;; sow :key-bindings
  (get-in @j/*line-reader* [:key-bindings :main])

  ;; terminal doesn't print keys right... but emacs does??
  (->> @j/*line-reader*
       ::j/key-bindings
       :emacs
       (map (juxt (comp #(KeyMap/display %) first) second))
       (pprint))

  (->> @j/*line-reader*
       ::j/key-bindings
       :emacs
       (map (juxt (comp #(map int %) first) second))
       (pprint))
  ;; ^X^D is (24 4)
  (-> (apply str (map char [24 4]))
      (KeyMap/display)))

;; home is 71 end is 79
;; https://www.lookuptables.com/coding/keyboard-scan-codes

;; make ctl-X ctl-X beginning-of-buffer
(swap! j/*line-reader* update-in  [:key-bindings :emacs]
   #((fnil conj []) %1 (first %2))
   [(apply str (map char [24 24])) "beginning-of-buffer"])
(j/apply-key-bindings!)


(let [km (.get (.getKeyMaps line-reader) "emacs")]
     (.readBinding line-reader km))
