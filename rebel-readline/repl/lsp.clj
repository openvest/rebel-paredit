(ns lsp
  #_(:require))


;; rewrite-clj ;; used in clerk cljfmt clojure-lsp and others
;; ;; uses zippers
;;   https://github.com/clj-commons/rewrite-clj
;; cljfmt
;; ;; used in rebel-readline (deferred loading of this)
;;   https://github.com/weavejester/cljfmt
;; clj-kondo
;;   https://github.com/clj-kondo/clj-kondo
;; clojure-lsp
;; ;; uses rewrite-clj and clj-kondo
;;   https://github.com/clojure-lsp/clojure-lsp

;; colorizing specific
;; puget
;;  https://github.com/greglook/puget
;; note that rebel-readline has its own tokenizer and colorizer
(require '[rewrite-clj.parser :as p]
         '[rewrite-clj.node :as n])
