(ns repl-balance.clojure.paredit-reference-card-test
  "These examples are from the Paredit Reference Card pdf for version 22
  some examples are `clojureized` some are modified to suit emacs behavior
  or other differences as noted.
  No additional tests are added; look to the other test modules for more
  edge case testing"
  (:require [repl-balance.clojure.paredit :as SUT]
            [repl-balance.core]
            [repl-balance.test-helpers :refer [s-cur-test buf-test]]
            [clojure.test :refer :all]))

;;;;;;;;;; Basic Insertion Commands ;;;;;;;;;;
#_(deftest ^:autopair paredit-open-round
  "(a b |c d)"
  "(a b (|) c d)"

  "(foo \"bar |baz\" quux)"
  "(foo \"bar (|baz\" quux)"
  )

#_(deftest ^:autopair paredit-close-round
  "(a b |c  )"
  "(a b c)|"

  "; Hello,| world!"
  "; Hello,)| world!"
  )

#_(deftest ^:autopair paredit-close-round-and-newline
  "Not sure about this example. It's more lispy than clojure
  Maybe work with square brackets?"
  "(defn f (x|  ))"
  "(defn f (x)\n  |)"
  )

#_(deftest ^:autopair paredit-open-square

  "(a b |c d)"
  "(a b [|] c d)"

  "(foo \"bar |baz\" quux)"
  "(foo \"bar [|baz\" quux)"
  )

#_(deftest ^:autopair paredit-close-square
  "(define-key keymap [frob|  ] 'frobnicate)"
  "(define-key keymap [frob]| 'frobnicate)"
  )

#_(deftest ^:autopair paredit-doublequote

  "(frob grovel |full lexical)"
  "(frob grovel \"|\" full lexical)"

  "(foo \"bar |baz\" quux)"
  "(foo \"bar \"|baz\" quux)"
  )

#_(deftest ^:autopair paredit-meta-doublequote

  "(foo \"bar |baz\" quux)"
  "(foo \"bar baz\"| quux)"  ;; emacs behavior
  ;; "(foo \"bar baz\"\n    |quux)"  ;; ref card
  )

#_(deftest paredit-backslash
  "example modified for clojure
  Note: is invalid sexp until the next character is typed"
  "(str )"
  "(str \\)"
  )

#_(deftest paredit-comment-dwim
  "This is a bit of whose right/choose your own adventure
  paredit-ref-card, emacs and cursive all have different behaviors"
  "(foo |bar) ; baz"
  "(foo bar) ; |baz"

  "(frob grovel)|"
  "(frob grovel) ;|"

  "|(defn hello-world ...)"
  ";;; |\n  (defn hello-world ...)"
  )

#_(deftest paredit-newline
  "not much diff from default behavior except perhaps indenting
  Note: example modified for clojure"
  "(let [s frobnicate] | (str (inc n)"
  "(let [s frobnicate]\n  |(str (inc n)"
  )

;;;;;;;;;; Deleting & Killing ;;;;;;;;;;

#_(deftest paredit-forward-delete
  (s-cur-test SUT/paredit-delete-forward
              "(quu|x \"zot\")"
              "(quu| \"zot\")"

              "(quux |\"zot\")"
              "(quux \"|zot\")"

              "(quux \"|zot\")"
              "(quux \"|ot\")"

              "(foo (|) bar)"
              "(foo | bar)"

              "|(foo bar)"
              "(|foo bar)"))

#_(deftest paredit-delete-backward
    "(\"zot\" q|uux)"
    "(\"zot\" |uux)"

    "(\"zot\"| quux)"
    "(\"zot|\" quux)"

    "(\"zot|\" quux)"
    "(\"zo|\" quux)"

    "(foo (|) bar)"
    "(foo | bar)"

    "(foo bar)|"
    "(foo bar|)"
  )

(deftest paredit-kill
  (s-cur-test SUT/kill
              "(foo bar)|  ;; Useless Comment"
              "(foo bar)|")
  (s-cur-test SUT/kill
              "(|foo bar)  ;; Useless Comment"
              "(|)  ;; Useless Comment")
  (s-cur-test SUT/kill
              "|(foo bar)  ;; Useless Line"
              "|"))

(deftest paredit-kill-in-string
  ;; FIXME: breaking and breaks balance.
  (s-cur-test SUT/kill
              "(foo \"|bar baz\"\n     quux)"
              "(foo \"|\"\n     quux)"))

#_(deftest paredit-forward-kill-word
  )

#_(deftest paredit-backward-kill-word
  )

;;;;;;;;;; Movement & Navigation ;;;;;;;;;;

(deftest paredit-forward
  (buf-test SUT/forward
            "(foo |(bar baz) quux)"
            "(foo (bar baz)| quux)")
  (buf-test SUT/forward
            "(foo (bar)|)"
            "(foo (bar))|"))

(deftest paredit-backward
  (buf-test SUT/backward
            "(foo (bar baz)| quux)"
            "(foo |(bar baz) quux)")
  (buf-test SUT/backward
            ;; this is emacs behavior, minor diff from ref card
            "(| (foo) bar)"
            "|( (foo) bar)"))

;;;;;;;;;; Depth-Changing Commands ;;;;;;;;;;

(deftest paredit-wrap-round
  (buf-test SUT/open-and-slurp
    "(foo |bar baz)"
    "(foo (|bar) baz)"))

(deftest ^:cursor-pos paredit-splice-sexp
  (buf-test SUT/splice
            "(foo (bar| baz) quux)"
            "(foo bar| baz quux)"))

#_(deftest paredit-splice-sexp-killing-backward
  )

#_(deftest paredit-splice-sexp-killing-forward
  )

(deftest paredit-raise-sexp
  ;; FIXME: this works but the key binding fails.
  (buf-test SUT/raise
            ;; removed a \n here for test output clarity
            "(dynamic-wind in (fn[] |body) out)"
            "(dynamic-wind in |body out)"))

;;;;;;;;;; Barfage & Slurpage ;;;;;;;;;;

(deftest ^:whitespace paredit-forward-slurp-sexp
  (buf-test SUT/slurp-forward
            "(foo (bar |baz) quux zot)"
            "(foo (bar |baz quux) zot)")
  (buf-test SUT/slurp-forward
            "(a b ((c | d)) e f)"
            "(a b ((c| d) e) f)"))

(deftest paredit-forward-barf-sexp
  (buf-test SUT/barf-forward
            "(foo (bar |baz quux) zot)"
            "(foo (bar |baz) quux zot)"))

(deftest paredit-backward-slurp-sexp
  ;; different cursor position than ref card but consistent with emacs
  (buf-test SUT/slurp-backward
            "(foo bar (baz| quux) zot)"
            "(foo (bar baz| quux) zot)")
  ;; reference card looks wrong targeting "(a (b (c| d) e) f)"
  (buf-test SUT/slurp-backward
            "(a b ((c| d)) e f)"
            "(a (b (c| d)) e f)"))

(deftest paredit-backward-barf-sexp
  (buf-test SUT/barf-backward
            "(foo (bar baz |quux) zot)"
            "(foo bar (baz |quux) zot)"))

;;;;;;;;;; Miscellaneous Commands ;;;;;;;;;;

(deftest ^:cursor-pos paredit-split-sexp
  (buf-test SUT/split
            "(hello| world)"
            "(hello)| (world)")
  (buf-test SUT/split
            "\"Hello, |world!\""
            "\"Hello, \"| \"world!\""))

#_(deftest paredit-join-sexp
    "(hello)| (world)"
    "(hello| world)"

    "\"Hello, \"| \"world!\""
    "\"Hello, |world!\""

    "hello-\n    | world"
    "hello-|world"
  )

#_(deftest paredit-recentre-on-sexp
  "no examples on ref card"
  )

#_(deftest paredit-reindent-defn
  "no examples on ref card"
  )

