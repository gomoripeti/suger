# suger
This repo is currently only a playground for experiments on syntactic sugar, parse transforms
and compile-time optimisations for Erlang.

- pipe/2: syntactic sugar that calls a sequence of functions to modify state
  avoiding using temporary variable names or funs.
  (a similar thing called chain operator [here](https://github.com/mad-cocktail/chacha))
