My solutions for Advent of Code
=====

Build & run
-----

If you want to run this project, make sure you have [rebar3](http://www.rebar3.org/) installed.
It's the de-facto toolchain for Erlang nowadays, so you really shouldn't be using anything else.
Use the Makefile with the build/run/clean targets, or manually do these things:

    $ rebar3 escriptize
    $ set -m && clear && _build/default/bin/aoc

Using command-line arguments, a specific year or day can be specified to run.
The data set (real data or sample data) can be chosen using the `mode` argument:

    $ make run [year X] [day X] [mode data/sample]
    $ make run year 2021 day 3 mode sample
