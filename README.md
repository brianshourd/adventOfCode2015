# Advent of Code 2015

My solutions to the [http://adventofcode.com](http://adventofcode.com) 2015
problems, in Haskell. Why? For fun, of course, and to play with this `stack`
program I keep hearing so much about. For the record, `stack` is pretty great.

To run these, install [stack](http://haskellstack.org), then run

```
stack setup
stack build adventOfCode --exec main
```

If you (by which I probably mean me in the future) want to actively work on
them, here are some tips.

1. Stack has a great feature that monitors the filesystem and continuously
   rebuilds and reruns your tests. This means that your tests must be fast
enough for this to matter. To achieve this, there is a separate project which
just has symlinks to the specific tests I am working on: `currentTests`.

```
stack test currentTests --fast --file-watch
```

This will keep me posted on build failures and test failures, and is an awesome
way to do some incremental TDD.
