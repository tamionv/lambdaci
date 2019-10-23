This project is an interpreter for the lambda calculus.
To run, just do a `cabal v2-run` command.
The following syntax is available:

* `leftmost reduce term`: performs one step of leftmost reduction on `term`
* `fully leftmost reduce term`: performs leftmost reduction until it is no longer possible on `term`
* `head reduce term`: performs one step of head reduction on `term`
* `fully head reduce term`: performs head reduction until it is no longer possible on `term`
* `type term`: tries to type `term`
* `type proof term`: tries to type `term`, outputting a type proof
* `ident = term`: sets `ident` to `term`
