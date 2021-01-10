## ffipf - Fuzzy Find in Project Fast (provisional)

**WARNING**: This is an alpha (at best) release and so it's very rough around
the edges. The name will also most likely change. There are hardcoded values
specific to my setup all over the place. Currently only working on Linux.

### What is it?

A dynamic Emacs module for finding files in directory hierarchies. Compiled for
performance and written in Nim.

### How to use

Checkout the repository. Make sure you have [Nim](https://nim-lang.org/)
installed, then do:

    make ffip.so

in the shell. Once built, add this directory and `elisp` subdirectory to your
`load-path`. Then evaluate:

    (load "ffip.so")
    (load "ffip.el")
    (ffip)
