## FFIPF - Fuzzy Find in Project Fast (provisional)

**WARNING**: This is an alpha (at best) release and so it's very rough around
the edges. The name will also most likely change. Currently only working on
Linux.

### What is it?

A dynamic Emacs module for finding files in directory hierarchies. Compiled for
performance and written in Nim.

### How to use

Checkout the repository. Make sure you have [Nim](https://nim-lang.org/)
installed, then do:

    make dist

in the shell. This will compile the module, run the (very basic right now) test,
and copy module to `./dist/`.

Once built, add the `./dist/` folder to your `load-path`, then evaluate
something like this:

```elisp
    (add-to-list 'load-path ".../ffipf/dist/")
    (defvar my-ffipf-loaded nil)
    (defun my-jump-file ()
      (interactive)
      (when (not my-ffipf-loaded)
        (load "ffipf_backend.so")
        (load "ffipf.el")
        (setq my-ffipf-loaded t))
      (ffipf))
```

then `M-x my-jump-file` when visiting a buffer placed in a project (basically,
anywhere the `project.el` can find out the root of).

You should get something like this:

![screenshot](https://raw.githubusercontent.com/piotrklibert/ffipf/master/img/screenshot.png)


### It doesn't work!

Well, that's expected, really. This is currently a Proof of Concept stage - it
works for me, I've started using it instead of `counsel-jump-file` from Ivy, but
that doesn't mean it will just work for anyone else! I plan to polish the
package in the upcoming weeks.

That being said, please open an issue if you decide to give it a try anyway! It
will help with my motivation to keep developing this :)
