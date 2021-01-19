import os
import sequtils
import fuzzy_file_finder

import emacs_types
import emacs_module
import emacs_helpers



module_init("ffipf-backend")


defun(init, max_args=1):
  reset_paths()
  let root = get_string(args[0])
  return init_paths(root.parentDir, root).toEmacs()


emacs_module.defun(search, max_args=1):
  let pattern = get_string(args[0])
  let emacs_strings = search(pattern)
                        .mapIt(it.res)
                        .map(toEmacs)
  return apply("list", emacs_strings)


provide()
