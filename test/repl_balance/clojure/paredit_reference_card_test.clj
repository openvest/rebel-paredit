(ns repl-balance.clojure.paredit-reference-card-test
  "These examples are from the Paredit Reference Card pdf for version 22
  some examples are `clojureized` some are modified to suit emacs behavior
  or other differences as noted.
  No additional tests are added; look to the other test modules for more
  edge case testing"
  (:require [repl-balance.clojure.paredit :as SUT]
            [repl-balance.core]
            [repl-balance.test-helpers :refer [s-cur-test buf-test reader-test]]
            [clojure.test :refer :all]))

;;;;;;;;;; Basic Insertion Commands ;;;;;;;;;;
(deftest paredit-open-round
  (s-cur-test SUT/open-round
              "(a b |c d)"
              "(a b (|) c d)")
  (s-cur-test SUT/open-round
              "(foo \"bar |baz\" quux)"
              "(foo \"bar (|baz\" quux)"))

(deftest paredit-close-round
  (reader-test SUT/close-round ")"
              "(a b |c  )"
              "(a b c)|")
  (reader-test SUT/close-round "]"
              "; Hello,| world!"
              "; Hello,]| world!"))

#_(deftest ^:autopair paredit-close-round-and-newline
  "Not sure about this example. It's more lispy than clojure
  Maybe work with square brackets?"
  "(defn f [x|  ])"
  "(defn f [x]\n  |)"
  )

(deftest paredit-open-square
  (s-cur-test SUT/open-square
              "(a b |c d)"
              "(a b [|] c d)")
  (s-cur-test SUT/open-square
              "(foo \"bar |baz\" quux)"
              "(foo \"bar [|baz\" quux)"))

(deftest paredit-close-square
  (s-cur-test SUT/close-round
              "(define-key keymap [frob|  ] 'frobnicate)"
              "(define-key keymap [frob]| 'frobnicate)"))

;; tests for open and close curly are not part of the paredit reference cardinal
;; but are a necessary addition for clojure
(deftest paredit-open-curly
  (s-cur-test SUT/open-curly
              "(a b |c d)"
              "(a b {|} c d)")
  (s-cur-test SUT/open-curly
              "(foo \"bar |baz\" quux)"
              "(foo \"bar {|baz\" quux)"))

(deftest paredit-close-square
  (buf-test SUT/close-round
              "(define-key keymap [frob|  ] 'frobnicate)"
              "(define-key keymap [frob]| 'frobnicate)"))

(deftest paredit-doublequote
  (s-cur-test SUT/doublequote
              "(frob grovel |full lexical)"
              "(frob grovel \"|\" full lexical)")
  (s-cur-test SUT/doublequote
              "(foo \"bar |baz\" quux)"
              "(foo \"bar \\\"|baz\" quux)"                 ;; need to fix how str-split-test counts cursor position??
              ))

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

(deftest paredit-comment-dwim
  "This is a bit of whose right/choose your own adventure.
  paredit-reference-card, emacs and cursive all have different behaviors
  The key here is to not break paren balance"
  (s-cur-test SUT/line-comment
              "(foo |bar) ;; baz"
              "(foo ;; |bar\n     ) ;; baz"
              #_emacs #_"(foo bar) ; |baz")
  (s-cur-test SUT/line-comment
              "(frob grovel)|"
              "(frob grovel) ;; |")
  (s-cur-test SUT/line-comment
              ;; differs from ref card but agrees with emacs and intellij
              ;; check on the white space on this later
              "|(defn hello-world ...)"
              ";; |(defn hello-world ...)"))
;(defn hello-world ...)
#_(deftest paredit-newline
  "not much diff from default behavior except perhaps indenting
  Note: example modified for clojure"
  "(let [s frobnicate] | (str (inc n)"
  "(let [s frobnicate]\n  |(str (inc n)"
  )

;;;;;;;;;; Deleting & Killing ;;;;;;;;;;


(deftest paredit-forward-delete
  (s-cur-test SUT/delete-char
              "(quu|x \"zot\")"
              "(quu| \"zot\")")
  ; in front of an opening doublequote
  (s-cur-test SUT/delete-char
              "(quux |\"zot\")"
              "(quux \"|zot\")")
  (s-cur-test SUT/delete-char
              "(quux \"|zot\")"
              "(quux \"|ot\")")
  ; in front of an closing doublequote
  (s-cur-test SUT/delete-char
              "(quux \"zot|\")"
              "(quux \"zot|\")")
  (s-cur-test SUT/delete-char
              "(foo (|) bar)"
              "(foo | bar)")
  (s-cur-test SUT/delete-char
              "|(foo bar)"
              "(|foo bar)"))

(deftest paredit-delete-backward
  (s-cur-test SUT/backward-delete-char
              "(\"zot\" q|uux)"
              "(\"zot\" |uux)")
  ;; backward facing a closing doublequote
  (s-cur-test SUT/backward-delete-char
              "(\"zot\"| quux)"
              "(\"zot|\" quux)")
  (s-cur-test SUT/backward-delete-char
              "(\"zot|\" quux)"
              "(\"zo|\" quux)")
  ;; backward facing an opening doublequote
  (s-cur-test SUT/backward-delete-char
              "(\"|zot\" quux)"
              "(\"|zot\" quux)")
  (s-cur-test SUT/backward-delete-char
              "(foo (|) bar)"
              "(foo | bar)")
  (s-cur-test SUT/backward-delete-char
              "(foo bar)|"
              "(foo bar|)"))

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

(deftest paredit-splice-sexp
  (buf-test SUT/splice
            "(foo (bar| baz) quux)"
            "(foo bar| baz quux)"))

#_(deftest paredit-splice-sexp-killing-backward
  )

#_(deftest paredit-splice-sexp-killing-forward
  )

(deftest paredit-raise-sexp
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

(deftest paredit-split-sexp
  (buf-test SUT/split
            "(hello| world)"
            "(hello)| (world)"))

(deftest paredit-split-string-test
  ;; FIXME: the reference card actually has this test which fails
  ;;        the rewrite-clj refuses to create new sequence nodes of type :forms
  ;; edge case:
  #_(buf-test SUT/split
            "\"Hello, |world!\""
            "\"Hello, \"| \"world!\"")
  (buf-test SUT/split
            "[\"Hello, |world!\"]"
            "[\"Hello, \"| \"world!\"]"))

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

