Agda integer addition shenanigans
=================================

My first experiment with [Agda](http://wiki.portal.chalmers.se/agda/pmwiki.php), a dependently typed functional programming language. I attempted to solve this problem:

> Write a program that adds two (or more) integers, without using any of the plus literal (+), the minus literal (-), the add method (or equivalent), the subtraction method (or equivalent), the sum method. The integers can be negative, zero or positive.

I'm not even a very good Haskell programmer so, so I definitely suck at Agda, but I had fun learning. I've cited borrowed code inline.

To run it:

0. Install Agda (on a Mac, try ```brew install agda```). If you don't install using homebrew, you might also need to install the [Agda Standard Library](https://github.com/agda/agda-stdlib).

0. If you installed using homebrew, you'll probably need to recompile the foreign function interface:

        cd /usr/local/lib/agda/ffi
        cabal install

0. Compile it:

        agda -i . -i <path to the agda std lib - on a mac try /usr/local/lib/agda/src> -c Main.agda

0. Run it:

        ./Main

