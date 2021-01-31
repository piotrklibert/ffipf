## FFIPF - Fuzzy Find in Project Fast (provisional)

**WARNING**: This is an alpha (at best) release and so it's very rough around
the edges. The name will also most likely change. Currently only working on
Linux.

### What is it?

A dynamic Emacs module for finding files in directory hierarchies. Compiled to
native and written in Nim.

The fuzzy matching and sorting algorithm is very well suited for navigation in
deeply nested hierachies of directories.

### How to use

Checkout the repository. Make sure you have [Nim](https://nim-lang.org/)
installed (warning: needs version 1.4+, probably too recent to be in your distro
package repository at the time of writing - Jan 2021), then do:

    make dist

in the shell. This will compile the module, run the (very basic right now)
tests, and copy compiled module along with Elisp library to `./dist/`.

**WARNING**: do not recompile the module while Emacs (more precisely: any Emacs
with ffipf loaded) is running. It may cause a segfault.

Once built, add the `./dist/` folder to your `load-path`, then evaluate
something like this:

```elisp
    (add-to-list 'load-path ".../ffipf/dist/")
    (require 'ffipf)
    (global-set-key (kbd "...") 'ffipf-jump-file)
```

then use `ffipf-jump-file` when visiting a buffer whose file lives inside a
project (basically, anywhere the `project.el` can find the root of).

You should get something like this:

![screenshot](https://raw.githubusercontent.com/piotrklibert/ffipf/master/img/screenshot.png)


### It doesn't work!

Well, that's expected, really. This is currently a Proof of Concept stage - it
works for me, I've started using it instead of `counsel-jump-file` from Ivy, but
that doesn't mean it will just work for anyone else! I plan to polish the
package in the upcoming weeks.

That being said, please open an issue if you decide to give it a try anyway! It
will help with my motivation to keep developing this :)
