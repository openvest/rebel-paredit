# repl-balance-cljs

[![Clojars Project](https://img.shields.io/clojars/v/com.openvest/repl-balance-cljs.svg)](https://clojars.org/com.openvest/repl-balance-cljs)

A library that supplies a rebel readline service for the default
clojurescript repl and some helpers to create CLJS repls with
repl-balance.

## Quick try

#### Clojure tools

If you want to try this really quickly [install the Clojure CLI tools](https://clojure.org/guides/getting_started) and then invoke this:

```shell
clojure -Sdeps '{:deps {com.openvest/repl-balance-cljs {:mvn/version "0.1.4"}}}' -m repl-balance.cljs.main
```

That should start a Nashorn ClojureScript REPL that takes it's input
from the Rebel readline editor.

Note that I am using the `clojure` command and not the `clj` command
because the latter wraps the process with another readline program (`rlwrap`).

#### Leiningen

Add `[com.openvest/repl-balance-cljs "0.1.4"]` to the dependencies in your
`project.clj` then start a REPL like this:

```shell
lein trampoline run -m repl-balance.cljs.main
```

#### Clone this repo

Clone this repo and then from the `repl-balance-cljs` sub-directory
typing `lein trampoline run -m repl-balance.cljs.main` will get you into
a Clojure REPL with the readline editor working.

Note that `lein run -m repl-balance.cljs.main` will not work!

## Usage

A simple usage example:

```clojure
(repl-balance.core/with-line-reader
  (repl-balance.clojure.core/create
    (repl-balance.cljs.service.local/create))
  (cljs.repl/repl
     :prompt (fn []) ;; prompt is handled by line-reader
     :read (repl-balance.cljs.repl/create-repl-read)))
```

## License

Copyright © 2018 Bruce Hauman

Distributed under the Eclipse Public License either version 1.0 or (at
your option) any later version.
