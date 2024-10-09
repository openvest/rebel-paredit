# repl-balance

[![Clojars Project](https://img.shields.io/clojars/v/com.openvest/repl-balance.svg?include_prereleases)](https://clojars.org/com.openvest/repl-balance)

A terminal readline library for Clojure Dialects.  Forked from rebel-readline this adds paredit fuctionality.

[![asciicast](https://asciinema.org/a/160597.png)](https://asciinema.org/a/160597)

## Why create a terminal readline library?

- no-editor required (no ide requirements for new clojure devs)
- work on remote machines
- work inside docker containers (where there's no editor with plugins)
- extend with clojure. not elisp, fennel, kotlin, vimscript or any other (inferior) langauage.


## Quick try

### Clojure tools

If you want to try this really quickly
[install the Clojure CLI tools](https://clojure.org/guides/getting_started)
and then invoke this:

```shell
clojure -Sdeps "{:deps {com.openvest/repl-balance {:mvn/version \"0.2.102\"}}}" -m repl-balance.main
```

That should start a Clojure REPL that takes its input from the ReplBalance readline editor.

Note that I am using the `clojure` command and not the `clj` command
because the latter wraps the process with another readline program (rlwrap).

Alternatively you can specify an alias in your `$HOME/.clojure/deps.edn`

```clojure
{
 ...
 :aliases {:repl/balance {:extra-deps {com.openvest/repl-balance {:mvn/version "0.2.102"}}
                          :main-opts  ["-m" "repl-balance.main"]}}
}
```

And then run with a simpler:

```shell
$ clojure -M:repl/balance
```

#### Leiningen

Add `[com.openvest/repl-balance "0.2.102"]` to the dependencies in your
`project.clj` then start a REPL like this:

```shell
lein trampoline run -m repl-balance.main
```

Alternatively, you can add repl-balance globally to `$HOME/.lein/profiles.clj`

```clojure
{
 ...
 :user {:dependencies [[com.openvest/repl-balance "RELEASE"]]}
}
```

Then you can call

```shell
lein trampoline run -m repl-balance.main
```

To make this less verbose you can use an alias in your `project.clj`:

```clojure
{
 ...
 :aliases {"repl-balance" ["trampoline" "run" "-m" "repl-balance.main"]}
}
```

Alternatively, you can do this globally in `$HOME/.lein/profiles.clj`:

```clojure
{
 ...
 :user {:aliases {"repl-balance" ["trampoline" "run" "-m" "repl-balance.main"]}}
}
```

Now you can start a repl-balance REPL with `lein repl-balance`.


## Quick Lay of the land

You should look at `repl-balance.clojure.main` and `repl-balance.core`
to give you top level usage information.

The core of the functionality is in
`repl-balance.clojure.line-reader` everything else is just support.

## Quick Usage

These are some quick examples demonstrating how to use the repl-balance
API.

The main way to utilize this readline editor is to replace the
`clojure.main/repl-read` behavior in `clojure.main/repl`.

The advantage of doing this is that it won't interfere with the input
stream if you are working on something that needs to read from
`*in*`. This is because the line-reader will only be engaged when the
REPL loop is reading.

Example:

```clojure
(repl-balance.core/with-line-reader
  (repl-balance.clojure.line-reader/create
    (repl-balance.clojure.service.local/create))
  (clojure.main/repl
     :prompt (fn []) ;; prompt is handled by line-reader
     :read (repl-balance.clojure.main/create-repl-read)))
```

Another option is to just wrap a call you your REPL with
`repl-balance.core/with-readline-in` this will bind `*in*` to an
input-stream that is supplied by the line reader.

```clojure
(repl-balance.core/with-readline-in
  (repl-balance.clojure.line-reader/create
    (repl-balance.clojure.service.local/create))
  (clojure.main/repl :prompt (fn[])))
```

Or with a fallback:

```clojure
(try
  (repl-balance.core/with-readline-in
    (repl-balance.clojure.line-reader/create
      (repl-balance.clojure.service.local/create))
    (clojure.main/repl :prompt (fn[])))
  (catch clojure.lang.ExceptionInfo e
    (if (-> e ex-data :type (= :repl-balance.jline-api/bad-terminal))
      (do (println (.getMessage e))
        (clojure.main/repl))
      (throw e))))
```

## Services

The line reader provides features like completion, documentation,
source, apropos, eval and more. The line reader needs a Service to
provide this functionality.

When you create a `repl-balance.clojure.line-reader`
you need to supply this service.

The more common service is the
`repl-balance.services.clojure.local` which uses the
local clojure process to provide this functionality and its a good
example of how a service works.

https://github.com/openvest/repl-balance/blob/master/src/repl_balance/clojure/service/local.clj


In general, it's much better if the service is querying the Clojure process
where the eventual REPL eval takes place.

However, the service doesn't necessarily have to query the same
environment that the REPL is using for evaluation. All the editing
functionality that rebel readline provides works without an
environment to query. And the apropos, doc and completion functionality is
still sensible when you provide those abilities from the local clojure process.

This could be helpful when you have a Clojurey REPL process and you
don't have a Service for it. In this case you can just use a
`clojure.service.local` or a `clojure.service.simple` service. If you
do this you can expect less than optimal results but multi-line
editing, syntax highlighting, auto indenting will all work just fine.


## nREPL, SocketREPL, pREPL?

Services have not been written for these REPLs yet!!

But you can use the `repl-balance.clojure.service.simple` service in the meantime.

## Contributing

Please contribute!

I'm trying to mark issues with `help wanted` for issues that I feel
are good opportunities for folks to help out. If you want to work on
one of these please mention it in the issue.

If you do contribute:

* if the change isn't small please file an issue before a PR.
* please put all PR changes into one commit
* make small grokable changes. Large changes are more likely to be
  ignored and or used as a starting issue for exploration.
* break larger solutions down into a logical series of small PRs
* mention it at the start, if you are filing a PR that is more of an
  exploration of an idea

I'm going to be more open to repairing current behavior than I will be
to increasing the scope of repl-balance.

I will have a preference for creating hooks so that additional functionality
can be layered on with libraries.


## License

Copyright © 2024 Philip Cooper

Distributed under the Eclipse Public License either version 1.0 or (at
your option) any later version.
