import macros
import os
import sequtils
import fuzzy_file_finder

import emacs_types
import emacs_module
import emacs_helpers



module_init("ffipf-backend")


defun(init, max_args=1):
  let root = get_string(args[0])
  reset_paths()
  let paths_count = init_paths(root.parentDir, root)
  return mk_num(paths_count)


emacs_module.defun(search, max_args=1):
  let pat = get_string(args[0])
  let res = search(pat)
  var emacs_res = res.mapIt(mk_string(it.res))
  echo funcall("current-thread")
  return funcall("list", emacs_res)


provide()
