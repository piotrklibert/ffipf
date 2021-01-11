import os
import sequtils
import fuzzy_file_finder

import emacs_types
import emacs_module
import emacs_helpers

init(emacs)
{. emit: "int plugin_is_GPL_compatible;" .}


emacs.defun(init, max_args=1):
  let root = get_string(env, args[0])
  reset_paths()
  let paths_count = init_paths(root.parentDir, root)
  return env.mk_num(paths_count)


emacs.defun(search, max_args=1):
  let pat = get_string(env, args[0])
  let res = search(pat)
  var emacs_res = res.mapIt(env.mk_string(it.res))
  return env.mk_call("list", emacs_res)


provide(emacs)
