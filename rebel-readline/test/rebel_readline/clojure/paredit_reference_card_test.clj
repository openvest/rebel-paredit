(ns rebel-readline.clojure.paredit-reference-card-test
  "These examples are from the Paredit Reference Card pdf for version 22
  some examples are `closureized` some are modified to suit emacs behavior
  or other differences as noted.
  No additional tests are added; look to the other test modules for more
  edge case testing"
  (:require [rebel-readline.clojure.paredit :as SUT]
            [rebel-readline.jline-api :as j]
            [rebel-readline.core]
            [rewrite-clj.zip :as z]
            [clojure.string :as str]
            [clojure.test :refer :all])
  (:import [org.jline.reader.impl LineReaderImpl BufferImpl]
           [org.jline.terminal TerminalBuilder]))

;;;;;;;;;; Basic Insertion Commands ;;;;;;;;;;
(deftest paredit-open-round
  "(a b |c d)"
  "(a b (|) c d)"

  "(foo \"bar |baz\" quux)"
  "(foo \"bar (|baz\" quux)"
  )

(deftest paredit-close-round
  "(a b |c  )"
  "(a b c)|"

  "; Hello,| world!"
  "; Hello,)| world!"
  )

(deftest paredit-close-round-and-newline
  "Not sure about this example. It's more lispy than clojure
  Maybe work with square brackets?"
  "(defn f (x|  ))"
  "(defn f (x)\n  |)"
  )

(deftest paredit-open-square
  "(a b |c d)"
  "(a b [|] c d)"

  "(foo \"bar |baz\" quux)"
  "(foo \"bar [|baz\" quux)"

  )

(deftest paredit-close-square
  "(define-key keymap [frob|  ] 'frobnicate)"
  "(define-key keymap [frob]| 'frobnicate)"
  )

(deftest paredit-doublequote
  "(frob grovel |full lexical)"
  "(frob grovel \"|\" full lexical)"

  "(foo \"bar |baz\" quux)"
  "(foo \"bar \"|baz\" quux)"
  )

(deftest paredit-meta-doublequote
  "(foo \"bar |baz\" quux)"
  "(foo \"bar baz\"| quux)"  ;; emacs behavior
  ;; "(foo \"bar baz\"\n    |quux)"  ;; ref card
  )

(deftest paredit-backslash
  "example modified for clojure
  Note: is invalid sexp until the next character is typed"
  "(str )"
  "(str \\)"
  )

(deftest paredit-comment-dwim
  "This is a bit of whose right/choose your own adventure
  paredit-ref-card, emacs and cursive all have different behaviors"
  "(foo |bar)"
  )

(deftest paredit-newline
  "not much diff from default behavior except perhaps indenting
  Note: example modified for clojure"
  "(let [s frobnicate] | (str (inc n)"
  "(let [s frobnicate]\n  |(str (inc n)"
  )

;;;;;;;;;; Deleting & Killing
(deftest paredit-forward-delete
  )

(deftest paredit-backward-delete
  )

(deftest paredit-kill
  )

(deftest paredit-forward-kill-word
  )

(deftest paredit-backward-kill-word
  )

;;;;;;;;;; Movement & Navigation ;;;;;;;;;;
(deftest paredit-forward
  )

(deftest paredit-backward
  )

;;;;;;;;;; Depth-Changing Commands ;;;;;;;;;;
(deftest paredit-wrap-round
  )

(deftest paredit-splice-sexp
  )

(deftest paredit-splice-sexp-killing-backward
  )

(deftest paredit-splice-sexp-killing-forward
  )

(deftest paredit-rise-sexp
  )

;;;;;;;;;; Barfage & Slurpage ;;;;;;;;;;
(deftest paredit-forward-slurp-sexp
  )

(deftest paredit-forward-barf-sexp
  )

(deftest paredit-backward-slurp-sexp
  )

(deftest paredit-backward-barf-sexp
  )

;;;;;;;;;; Miscellaneous Commands ;;;;;;;;;;
(deftest paredit-split-sexp
  )

(deftest paredit-join-sexp
  )

(deftest paredit-recentre-on-sexp
  "no examples on ref card"
  )

(deftest paredit-reindent-defn
  "no examples on ref card"
  )

