# repl-balance

[![Clojars Project](https://img.shields.io/clojars/v/com.openvest/repl-balance.svg?include_prereleases)](https://clojars.org/com.openvest/repl-balance)
[![cljdoc badge](https://cljdoc.org/badge/com.openvest/repl-balance)](https://cljdoc.org/d/com.openvest/repl-balance/)

**No** you **DO NOT** need an ide(s)+plugins or an editor+packages to have a positive repl experience.

`repl-balance` is a terminal readline library for Clojure Dialects.  Forked from rebel-readline this adds paredit fuctionality.

<!--
[![asciicast](https://asciinema.org/a/160597.png)](https://asciinema.org/a/160597)
-->
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
clojure -Sdeps "{:deps {com.openvest/repl-balance {:mvn/version \"0.2.114\"}}}" -m repl-balance.main
```

That should start a Clojure REPL that takes its input from the ReplBalance readline editor.

Note that I am using the `clojure` command and not the `clj` command
because the latter wraps the process with another readline program (rlwrap).

Alternatively you can specify an alias in your `$HOME/.clojure/deps.edn`

```clojure
{
 ...
 :aliases {:repl/balance {:extra-deps {com.openvest/repl-balance {:mvn/version "0.2.114"}}
                          :main-opts  ["-m" "repl-balance.main"]}}
}
```

And then run with a simpler:

```shell
$ clojure -M:repl/balance
```
## User docs
Additional user documentation is in the [project wiki](https://github.com/openvest/repl-balance/wiki/User-Documentation.)
## Paredit
Checkout the paredit docs page but if you are familiar with emacs paredit commands
you should be off and running without additional help

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

This could be helpful when you have a Clojure REPL process and you
don't have a Service for it. In this case you can just use a
`clojure.service.local` or a `clojure.service.simple` service. If you
do this you can expect less than optimal results but multi-line
editing, syntax highlighting, auto indenting will all work just fine.




## Contributing

Please contribute!

I'm trying to mark issues with `help wanted` for issues that I feel
are good opportunities for folks to help out. If you want to work on
one of these please mention it in the issue.

Most desired needs for contributors are:

  * A vim user to help add vim bindings for the paredit widgets (fireplace or conjure anyone?)
  * An OSX user to help create and test OSX bindings (I have osx but with my own os level key bindings and a custom keyboard)
  * Any nrepl or java threading help (java is not a language I know well)


## License

Copyright © 2024 Philip Cooper

Distributed under the Eclipse Public License either version 1.0 or (at
your option) any later version.
